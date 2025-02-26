(ns ied.events
  (:require
   [re-frame.core :as re-frame]
   [ied.config :as config]
   [ied.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   [ied.nostr :as nostr]
   [day8.re-frame.http-fx]
   [promesa.core :as p]
   [wscljs.client :as ws]
   [wscljs.format :as fmt]
   [clojure.string :as str]
   [clojure.set :as set]
   [superstructor.re-frame.fetch-fx]
   [ajax.core :as ajax]

   ["js-confetti" :as jsConfetti]
   [js-confetti :as jsConfetti]))

(re-frame/reg-event-db
 ::initialize-db
 (fn-traced [_ _]
            db/default-db))

(re-frame/reg-event-fx
 ::navigate
 (fn-traced [_ [_ handler]]
            {:dispatch [::set-visit-timestamp]
             :navigate handler}))

(re-frame/reg-event-fx
 ::set-active-panel
 (fn-traced [{:keys [db]} [_ active-panel]]
            {:db (assoc db :active-panel active-panel)}))

(re-frame/reg-event-fx
 ::set-route
 (fn-traced [{:keys [db]} [_ route]]
            {:db (assoc db :route route)}))

(def confetti-instance
  (new jsConfetti))

(re-frame/reg-fx
 ::add-confetti
 (fn [cofx _]
   (let [visited-at (-> cofx :db :visited-at)
         now (quot (.now js/Date) 1000)
         diff (- now visited-at)]
     (when (-> cofx :db :confetti)
       (when (>= diff 5)
         (.addConfetti confetti-instance (clj->js {:emojis ["😺" "🐈‍⬛" "🦄"]}))))
     {})))

(re-frame/reg-fx
 ::relay-list
 (fn [event]
   (when (= 30002 (:kind event))
     (.log js/console "got a relay list!"))))

(defn insert-event-in-events [event events]
  (.log js/console "Inserting event" (clj->js event))
  (let [event-id (:id event)]
    (if-let [existing-event (first (filter #(= (:id %) event-id) events))]
      (let [existing-relays (set (:relays existing-event))
            new-relays (set (:relays event))
            combined-relays (into existing-relays new-relays)]
        ;; Only update if there are new relays
        (if (= (count existing-relays) (count combined-relays))
          ;; No new relays, return unchanged events
          events
          ;; New relays found, update the event and resort
          (let [updated-event (assoc existing-event :relays (vec combined-relays))
                events-without-existing (remove #(= (:id %) event-id) events)]
            (into (sorted-set-by
                   (fn [a b] (compare (:created_at a) (:created_at b)))
                   updated-event)
                  events-without-existing))))
      ;; If the event doesn't exist yet, just add it to the collection
      (into (sorted-set-by
             (fn [a b] (compare (:created_at a) (:created_at b)))
             event)
            events))))

;; Database Event?
(re-frame/reg-event-fx
 ::save-event
 (fn-traced [{:keys [db]} [_ [uri raw-event]]]
            (when (and
                   (= (first raw-event) "EVENT"))
              (let [event (nth raw-event 2 raw-event)
                    updated-events (insert-event-in-events (merge event {:relays [uri]}) (:events db))]
                {:fx [[::add-confetti]
                      [::relay-list event]]
                 :db (merge db {:events updated-events})}))))

(defn handlers
  [ws-uri]
  ;; TODO store relay info with event
  {:on-message (fn [e] (re-frame/dispatch [::save-event [ws-uri (-> (.-data e)
                                                                    js/JSON.parse
                                                                    (js->clj :keywordize-keys true))]]))
   :on-open    #(re-frame/dispatch  [::load-events ws-uri])
   :on-close   #(prn "Closing a connection")
   :on-error   (fn [e] (.log js/console "Error with uri: " ws-uri (clj->js e))
                 (re-frame/dispatch  [::update-ws-connection-status ws-uri "error"]))})

(re-frame/reg-event-db
 ::update-ws-connection-status
 (fn [db [_ ws-uri status]]
   (let [target-ws (first (filter #(= ws-uri (:uri %)) (:sockets db)))]
     (assoc db
            :sockets
            (assoc (:sockets db)
                   (.indexOf (:sockets db) target-ws)
                   (merge target-ws {:status status}))))))

(re-frame/reg-event-fx
 ::load-events
 (fn-traced [cofx [_ ws-uri]]
            {::load-events-fx [ws-uri (-> cofx :db :sockets)]}))

(re-frame/reg-fx
 ::load-events-fx
 (fn [[ws-uri sockets]]
   (println "loading events")
   (let [target-ws (first (filter #(= ws-uri (:uri %)) sockets))]
     (ws/send (:socket target-ws) ["REQ" "424242" {:kinds [30004 30142]
                                                   :limit 100}] fmt/json))))

(defn create-socket
  [uri]
  (println "creating socket with uri" uri)
  (ws/create uri (handlers uri)))

(re-frame/reg-event-fx
 ::create-websocket
 ;; ws {:id uuid
 ;;     :uri
 ;;     :name
 ;;     :status connected | disconnected | error}
 ;; FIXME this needs a lot of rework
 (fn-traced [{:keys [db]} [_ ws]]
            (if (some #(= (:uri ws) (:uri %)) (:sockets db))
              (doall
               (println "uri already there" (:uri ws))
               {:db (assoc db
                           :sockets
                           (assoc (:sockets db)
                                  (.indexOf (:sockets db) ws)
                                  (merge ws {:socket (create-socket (:uri ws))
                                             :type ["outbox" "inbox"]
                                             :status "connected"})))})
              (doall
               (println "uri not yet known")
               {:db (update db :sockets conj (merge ws {:socket (create-socket (:uri ws))
                                                        :type ["outbox" "inbox"]
                                                        :status "connected"}))}))))

(re-frame/reg-event-fx
 ::connect-to-websocket
 (fn-traced [{:keys [db]} [_ ws-uri]]
            {::connect-to-websocket-fx [ws-uri (:sockets db)]}))

(re-frame/reg-fx
 ::connect-to-websocket-fx
 (fn [[ws-uri sockets]]
   (let [target-ws (first (filter #(= ws-uri (:uri %)) sockets))]
     (re-frame/dispatch [::create-websocket target-ws]))))

;; TODO use id to close socket
;; add connection status to socket
;; render connect / disconnect button based on status
(re-frame/reg-event-fx
 ::close-connection-to-websocket
 (fn-traced [{:keys [db]} [_ ws-uri]]
            {::close-connection-to-websocket-fx [ws-uri (:sockets db)]}))

(re-frame/reg-fx
 ::close-connection-to-websocket-fx
 (fn [[ws-uri  sockets]]
   (ws/close (:socket (first (filter #(= ws-uri (:uri %)) sockets))))
   (re-frame/dispatch [::update-ws-connection-status ws-uri "disconnected"])))

(re-frame/reg-event-fx
 ::connect-to-default-relays
 (fn-traced [cofx [_ default-relays]]
            {::connect-to-default-relays-fx default-relays}))

(re-frame/reg-fx
 ::connect-to-default-relays-fx
 (fn [default-relays]
   (doall
    (for [r default-relays]
      (re-frame/dispatch [::create-websocket r])))))

;; TODO 
(re-frame/reg-event-fx
 ::publish-signed-event
 (fn-traced [cofx [_ signedEvent]]
            (let [sockets (filter
                           (fn [s]
                             (some #(= "outbox"  %)
                                   (:type s)))
                           (-> cofx :db :sockets))
                  _ (.log js/console "available sockets " (clj->js sockets))]
              {::send-to-relays-fx [sockets signedEvent]})))

(re-frame/reg-fx
 ::send-to-relays-fx
 (fn [[sockets signedEvent]]
   (let [connected-sockets (filter #(= "connected" (:status %)) sockets)]
     (doseq [socket connected-sockets]
       (ws/send (:socket socket) ["EVENT" signedEvent] fmt/json)))))

(re-frame/reg-event-db
 ::update-websockets
 (fn [db [_ sockets]]
   (assoc db :sockets sockets)))

(re-frame/reg-event-fx
 ::remove-websocket
 (fn-traced [{:keys [db]} [_ socket]]
            {::remove-websocket-fx [(:id socket) (:sockets db)]}))

(re-frame/reg-fx
 ::remove-websocket-fx
 (fn [[id sockets]]
   (let [filtered (filter #(not= id (:id %)) sockets)] ;; TODO maybe this can also be done using the URI
     (re-frame/dispatch [::update-websockets filtered]))))

(re-frame/reg-event-db
 ::toggle-show-add-event
 (fn [db _]
   (assoc db :show-add-event (not (:show-add-event db)))))

;; TODO just pass an event here that is then passed on to signing
(re-frame/reg-event-fx
 ::publish-resource
 [(re-frame/inject-cofx  :now)]
 (fn-traced [cofx _]
            ;; TODO hier muss ich den ansatz ändern, es ist vmtl sinnvoller durch alle keys in md-form-resource zu iterieren und abhängig davon die tags zu bauen
            ;; -> vllt kann ich einfach eine Funktion bauen, die md-form resource amb-konform verwandet und dann die funktion zur publikation von amb daten recyclen
            (let [form-data (-> cofx :db :md-form-resource)
                  about (map (fn [e] ["about"
                                      (:id e)
                                      (second (first (filter (fn [l]
                                                               (= :de (first l)))
                                                             (:prefLabel e))))
                                      "de"])
                             (:about form-data)) ;; TODO this should be abstracted in a nostr-make-event-kind-function or sth
                  tags [["d" (:uri form-data)]
                        ["id" (:uri form-data)]
                        ["author" "" (:author form-data)]
                        ["name" (:name form-data)]]
                  event {:kind 30142
                         :created_at (:now cofx)
                         :content ""
                         :tags (into tags about)}
                  _ (.log js/console "Event to publish " (clj->js event))]
              {:navigate [:home]
               ::sign-and-publish-event [event (-> cofx :db :sk)]
               :db (assoc (:db cofx) :md-form-resource nil)})))

(defn sign-event [unsignedEvent sk]
  (if (nostr/valid-unsigned-nostr-event? unsignedEvent)
    (p/let [_ (js/console.log (clj->js unsignedEvent))
            signedEvent (if (nil? sk)
                          (.nostr.signEvent js/window (clj->js unsignedEvent))
                          (nostr/finalize-event unsignedEvent sk))
            _ (js/console.log "Signed event: " (clj->js signedEvent))]
      signedEvent)
    (.error js/console "Event is not a valid nostr event: " (clj->js unsignedEvent))))

;; TODO maybe we need some validation before publishing
(re-frame/reg-fx
 ::sign-and-publish-event
 (fn [[unsignedEvent sk]]
   (p/let [signedEvent (sign-event unsignedEvent sk)]
     (re-frame/dispatch [::publish-signed-event signedEvent]))))

;; TODO make login a multimethod and call it with either extension or anononymslouy keyword?
(re-frame/reg-event-fx
 ::login-with-extension
 (fn-traced [cofx [_ _]]
            {::login-with-extension-fx _}))

(re-frame/reg-fx
 ::login-with-extension-fx
 (fn [db _]
   (p/let [pk (.nostr.getPublicKey js/window)]
     (re-frame/dispatch [::save-pk pk])
     (re-frame/dispatch [::relay-list-for-pk pk]))))

(re-frame/reg-event-fx
 ::save-pk
 (fn-traced [{:keys [db]} [_ pk]]
            {:db (assoc db :pk pk)
             :dispatch [::get-lists-for-npub (nostr/get-npub-from-pk pk)]}))

(re-frame/reg-event-db
 ::logout
 (fn-traced [db _]
            (assoc db :pk nil :sk nil)))

(re-frame/reg-cofx
 :now
 (fn [cofx _data] ;; _data unused
   (assoc cofx :now (quot (.now js/Date) 1000))))

(re-frame/reg-cofx
 :sk
 (fn [cofx _]
   (assoc cofx :sk (:sk cofx))))

(re-frame/reg-cofx
 :pk
 (fn [cofx _]
   (assoc cofx :pk (:pk cofx))))

(defn convert-amb-to-nostr-event
  [parsed-json created_at]
  (let [tags (into [["d" (:id parsed-json)]
                    ["r" (:id parsed-json)]
                    ["id" (:id parsed-json)]
                    ["name" (:name parsed-json)]
                    ["description" (:description parsed-json "")]
                    (into ["keywords"] (:keywords parsed-json))
                    ["image" (:image parsed-json "")]]
                   cat [(map (fn [e] ["about" (:id e) (-> e :prefLabel :de) "de"]) (:about parsed-json)) ;; TODO fix the language parsing and make it generic
                        (map (fn [e] ["creator"
                                      (if-let [id  (get e :id)]
                                        id
                                        "")
                                      (:name e)])
                             (:creator parsed-json))
                        (map (fn [e] ["inLanguage" e]) (:inLanguage parsed-json))])
        event {:kind 30142
               :created_at created_at
               :content ""
               :tags tags}]
    event))

(re-frame/reg-event-fx
 ::convert-amb-string-and-publish-as-nostr-event
 [(re-frame/inject-cofx  :now)]
 (fn-traced [cofx [_ json-string]]
            (let [parsed-json (js->clj (js/JSON.parse json-string) :keywordize-keys true)
                  event (convert-amb-to-nostr-event parsed-json (:now cofx))]
              {::sign-and-publish-event [event (-> cofx :db :sk)]})))

(re-frame/reg-event-fx
 ::convert-amb-json-and-publish-as-nostr-event
 [(re-frame/inject-cofx  :now)]
 (fn-traced [cofx [_ result]]
            (let [json (:body result)
                  event (convert-amb-to-nostr-event json (:now cofx))]
              {::sign-and-publish-event [event (-> cofx :db :sk)]})))

(re-frame/reg-event-fx
 ::toggle-selected-events
 (fn [{:keys [db]} [_ event]]
   (let [selected-event-ids (set (map (fn [e] (:id e)) (:selected-events db)))]
     (if (and (seq (:selected-events db))
              (contains? selected-event-ids (:id event)))
       {:db (assoc db  :selected-events (filter #(not= (:id event) (:id %)) (:selected-events db)))}
       {:db (update db :selected-events conj event)}))))

(re-frame/reg-event-db
 ::toggle-selected-list-ids
 (fn [db [_ id]]
   (println "Toggle list ids: " id)
   (let [in-selected-list-ids (contains? (:selected-list-ids db) id)]
     (if in-selected-list-ids
       (update db :selected-list-ids disj id)
       (update db :selected-list-ids conj id)))))

(re-frame/reg-event-fx
 ::add-metadata-event-to-list
 [(re-frame/inject-cofx  :now)]
 (fn [cofx [_ [list resources-to-add]]]
   (let [existing-tags  (:tags list)
         existing-tags-set  (set existing-tags)
         tags-to-add (filter #(not (contains? existing-tags-set %))
                             (map (fn [e] (cond
                                            (= 1 (:kind e)) ["e" (:id e)]
                                            (= 30142 (:kind e)) (nostr/build-tag-for-adressable-event e)))
                                  resources-to-add))
         new-tags (vec (concat existing-tags tags-to-add))
         event {:kind 30004
                :created_at (:now cofx)
                :content ""
                :tags new-tags}]
     {::sign-and-publish-event [event (-> cofx :db :sk)]})))

(re-frame/reg-event-fx
 ::add-metadata-events-to-lists
 (fn [cofx [_ [events lists]]]
   (let [dispatch-events (mapv (fn [l] [::add-metadata-event-to-list [l events]]) lists)
         _ (.log js/console (clj->js dispatch-events))]
     {:fx [[:dispatch-n dispatch-events]]})))

(defn sanitize-subscription-id
  "cuts the subscription string at 64 chars"
  [s]
  (str/join "" (take 64 s)))

(defn make-sub-id [prefix id]
  (-> (str prefix id)
      (sanitize-subscription-id)))

(re-frame/reg-event-fx
 ::get-lists-for-npub
 (fn [{:keys [db]} [_ npub]]
   (let [query-for-lists ["REQ"
                          (make-sub-id "lists-" npub) ;; TODO maybe make this more explicit later
                          {:authors [(nostr/get-pk-from-npub npub)]
                           :kinds (concat (:follow-sets db) (:list-kinds db))}]
         sockets (:sockets db)]
     {::request-from-relay [sockets query-for-lists]
      :dispatch [::get-deleted-lists-for-npub [sockets npub]]})))

(re-frame/reg-event-fx
 ::relay-list-for-pk
 (fn [{:keys [db]} [_ pk]]
   (.log js/console "get relay list for pk" pk)
   (let [query-for-relay-list ["REQ"
                               (make-sub-id "relay-lists-" pk) ;; TODO maybe make this more explicit later
                               {:authors [pk]
                                :kinds [30002]}]
         sockets (:sockets db)]
     {::request-from-relay [sockets query-for-relay-list]})))

(re-frame/reg-event-fx
 ::get-follow-set-for-npub
 (fn [{:keys [db]} [_ npub]]
   (let [query-for-lists ["REQ"
                          (make-sub-id "follow-set-" npub) ;; TODO maybe make this more explicit later
                          {:authors [(nostr/get-pk-from-npub npub)]
                           :kinds (:follow-sets db)}]
         sockets (:sockets db)]
     {::request-from-relay [sockets query-for-lists]})))

(re-frame/reg-event-fx
 ::get-deleted-lists-for-npub
 (fn [cofx [_ [sockets npub]]]
   (let [query-for-deleted-lists ["REQ"
                                  (make-sub-id  "deleted-lists-" npub) ;; TODO maybe make this more explicit later
                                  {:authors [(nostr/get-pk-from-npub npub)]
                                   :kinds [5]}]]
     {::request-from-relay [sockets query-for-deleted-lists]})))

(re-frame/reg-event-fx
 ::events-from-pks-actor-follows
 (fn [{:keys [db]} [_ pks]]
   (.log js/console "requesting events from pks actor follows..")
   (let [query-for-events ["REQ"
                           (make-sub-id "events-from-pks-actor-follows" (:pk db))
                           {:authors pks
                            :kinds [1]
                            :limit 10}]] ;; TODO remember timestamps and use them in query
     {::request-from-relay [(:sockets db) query-for-events]})))

(re-frame/reg-fx
 ::request-from-relay
 (fn [[sockets query]]
   (doall
    (for [s (filter (fn [s] (= "connected" (:status s))) sockets)]
      (ws/send (:socket s) query fmt/json)))))

(re-frame/reg-event-fx
 ::query-for-event-ids
 (fn [db [_ [sockets event-ids]]]
   (let [query ["REQ"
                "RAND42"
                {:ids event-ids}]]
     {::request-from-relay [sockets query]})))

;; FIXME at some future point in time we should think about cancelling subscriptions after success
;; at least for these it might make sense
(re-frame/reg-event-fx
 ::query-for-d-tag
 (fn [db [_ [sockets d-tags]]]
   (let [query ["REQ"
                (sanitize-subscription-id (first d-tags)) ;;TODO guess this can be made more sensible
                {:#d d-tags}]]
     {::request-from-relay [sockets query]})))

(defn cleanup-list-name [s]
  (-> s
      (str/replace #"\s" "-")
      (str/replace #"[^a-zA-Z0-9]" "-")))

(re-frame/reg-event-fx
 ::create-new-list
 [(re-frame/inject-cofx  :now)]
 (fn [cofx [_ name]]
   (let [tags [["d" (cleanup-list-name name)]
               ["title" name]]
         create-list-event {:kind 30004
                            :created_at (:now cofx)
                            :content ""
                            :tags tags}]
     {::sign-and-publish-event [create-list-event (-> cofx :db :sk)]})))

(re-frame/reg-event-fx
 ::delete-list
 [(re-frame/inject-cofx  :now)]
 (fn [cofx [_ l]]
   (let [{:keys [list-kinds]} (:db cofx)
         all-list-kinds (concat list-kinds)
         deletion-event {:kind 5
                         :created_at (:now cofx)
                         :content ""
                         :tags [(cond
                                  (= 1 (:kind l))
                                  ["e" (:id l)]
                                  (some #{(:kind l)} all-list-kinds)
                                  ["a" (str (:kind l) ":" (:pubkey l) ":" (second (first (filter
                                                                                          #(= "d" (first %))
                                                                                          (:tags l)))))])]}]
     {::sign-and-publish-event [deletion-event (-> cofx :db :sk)]})))

(re-frame/reg-event-db
 ::toggle-show-lists-modal
 (fn [db _]
   (assoc db :show-lists-modal (not (:show-lists-modal db)))))

(re-frame/reg-event-db
 ::toggle-show-create-list-modal
 (fn [db _]
   (assoc db :show-create-list-modal (not (:show-create-list-modal db)))))

(re-frame/reg-event-db
 ::toggle-show-event-data-modal
 (fn [db [_ event]]
   (assoc db
          :show-event-data-modal (not (:show-event-data-modal db))
          :selected-event event)))

(re-frame/reg-event-fx
 ::delete-event-from-list
 [(re-frame/inject-cofx  :now)]
 (fn [cofx [_ [event list]]]
   (let [filtered-tags (filterv (fn [t] (not= (:id event) (nostr/extract-id-from-tag (second t)))) (:tags list))
         _ (println (:tags list))
         _ (.log js/console "Filtered Tags: " (clj->js filtered-tags))
         event {:kind 30004
                :created_at (:now cofx)
                :content ""
                :tags filtered-tags}]
     {::sign-and-publish-event [event (-> cofx :db :sk)]})))

(re-frame/reg-event-db
 ::create-sk
 (fn [db [_]]
   (let [sk (nostr/generate-sk)
         pk (nostr/get-pk-from-sk sk)]
     (assoc db :sk sk :pk pk))))

(re-frame/reg-fx
 ::get-amb-json-from-uri
 (fn [uri]
   (p/let [raw-html (js/fetch  uri {:headers {"Access-Control-Allow-Origin" "*"}})]
     (println raw-html))))

(re-frame/reg-event-fx
 ::publish-amb-uri-as-nostr-event
 (fn [db [_ uri]]
   {:fetch {:method :get
            :url uri
            :mode :cors
            :credentials :omit
            :timeout 8000
            :response-content-types {#"application/.*json" :json}
            :on-success [::convert-amb-json-and-publish-as-nostr-event]
            :on-failure (.log js/console "publishing amb uri as nostr did not work")}}))

(re-frame/reg-event-fx
 ::set-visit-timestamp
 [(re-frame/inject-cofx  :now)]
 (fn [cofx [_]]
   {:db (assoc (:db cofx) :visited-at (:now cofx))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get metadata from uri ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-uri? [s]
  (try
    (js/URL. s)
    true
    (catch :default e false)))

(re-frame/reg-event-fx
 ::try-get-metadata-from-uri
 (fn [cofx [_ uri]]
   (when (valid-uri? uri)
     (println "valid uri" (valid-uri? uri))
     {:dispatch [::get-metadata-from-json uri]})))

(re-frame/reg-fx
 ::try-get-metadata-from-uri-fx
 (fn [uri]
   (try
     (println "trying to get metadata from json")
     (re-frame/dispatch [::get-metadata-from-json uri])
     (catch :default e
       (println "... did not work..looking for script tag")
       (try
         (re-frame/dispatch [::get-metadata-from-script-tag uri])
         (catch :default e
           (js/console.error "all attempts to fetch something sensible failed.")))))))

(re-frame/reg-event-db
 ::prefill-metadata-form
 (fn [db [_ uri result]]
   (let [data (:body result)]
     (if (and
          (:id data)
          (:about data))
       (assoc db :resource-to-add data)
       (doall (js/console.error "Not the right kind of data")
              (re-frame/dispatch [::get-metadata-from-script-tag uri]))))))

(re-frame/reg-event-fx
 ::failure
 (fn [cofx _]
   (println "failure in xhrio request")))

(re-frame/reg-event-db
 ::parse-text-for-script-tag
 (fn [db [_ uri result]]
   (let [data (:body result)
         parser (js/DOMParser.)
         doc (.parseFromString parser data "text/html")
         script-tag (.querySelector doc "script[type='application/ld+json']")]
     (if script-tag
       (let [ld-json (js/JSON.parse (.-textContent script-tag))]
         (assoc db :resource-to-add (js->clj ld-json)))
       (println "did not get script tag")))))

(re-frame/reg-event-fx
 ::get-metadata-from-script-tag
 (fn [cofx [_ uri]]
   (println "now trying to get script tag..")
   {:fetch {:method :get
            :url uri
            :timeout 3000
            :response-content-types {#"application/.*json" :json}
            :on-success [::parse-text-for-script-tag uri]
            :on-failure [::failure]}}))

(re-frame/reg-event-fx
 ::get-metadata-from-json
 (fn [cofx [_ uri]]
   {:fetch {:method :get
            :url (if (str/ends-with? uri "json") ;; TODO elaborate this a bit more
                   uri
                   (str/replace uri #".html" ".json"))
            :timeout 3000
            :response-content-types {#"application/.*json" :json}
            :on-success [::prefill-metadata-form uri]
            :on-failure [::get-metadata-from-script-tag uri]}}))

(re-frame/reg-event-db
 ::save-concept-scheme
 (fn [db [_ result]]
   (let [cs (:body result)]
     (assoc-in db [:concept-schemes (:id cs)] cs))))

(comment
  (keyword "https://ww.googl-e.com/"))

(defn jsonize-uri
  [uri]
  (cond
    (str/ends-with? uri ".json")
    uri
    (str/ends-with? uri ".html")
    (str/replace uri #"\.\w{4}" ".json")
    :else
    (str uri ".json")))

(comment
  (jsonize-uri "https:/goo"))

(re-frame/reg-event-fx
 ::skos-concept-scheme-from-uri
 (fn [cofx [_ uri]]
   {:fetch {:method :get
            :mode :cors
            :credentials :omit
            :url (jsonize-uri uri)
            :timeout 5000
            :response-content-types {#"application/.*json" :json}
            :on-success [::save-concept-scheme]
            :on-failure [::failure]}}))

(re-frame/reg-event-fx
 ::fetch-missing-concept-scheme
 (fn [{:keys [db]} [_ missing-uri]]
   (let [known-concept-schemes (keys (get db :concept-schemes {}))]
     (if (some #(= missing-uri %) known-concept-schemes)
       {:db db}
       {:db db
        :fetch {:method :get
                :mode :cors
                :credentials :omit
                :url (jsonize-uri missing-uri)
                :timeout 5000
                :response-content-types {#"application/.*json" :json}
                :on-success [::save-concept-scheme]
                :on-failure [::failure]}}))))

(comment
  (empty? ()))

(re-frame/reg-event-db
 ::toggle-concept
 (fn [db [_ [concept field]]]
   (update-in db [:md-form-resource field] (fn [coll]
                                             (if (some #(= (:id concept) (:id %)) coll)
                                               (filter (fn [e] (not= (:id e) (:id concept))) coll)
                                               (conj coll (select-keys concept [:id :notation :prefLabel])))))))

(re-frame/reg-event-db
 ::handle-md-form-input
 (fn [db [_ [field-name field-value]]]
   (assoc-in db [:md-form-resource field-name] field-value)))

(re-frame/reg-event-db
 ::handle-md-form-array-input
 (fn [db [_ [field-name field-id field-value]]]
   (assoc-in db [:md-form-resource field-name field-id] field-value)))

(re-frame/reg-event-db
 ::handle-md-form-rm-input
 (fn [db [_ [field-name field-id]]]
   (update-in
    db
    [:md-form-resource field-name]
    dissoc
    field-id)))

(re-frame/reg-event-db
 ::handle-md-form-add-input
 (fn [db [_ [field-name]]]
   (assoc-in db [:md-form-resource field-name (random-uuid)] nil)))

(re-frame/reg-event-fx
 ::handle-search
 (fn [cofx [_ search-term]]
   (let [uri (str config/typesense-uri
                  "search?q="
                  search-term
                  "&query_by="
                  "name,about,description,creator")]
     {:fetch {:method :get
              :url uri
              :mode :cors
              :credentials :omit
              :headers {"x-typesense-api-key" "xyz"}
              :timeout 5000
              :response-content-types {#"application/.*json" :json}
              :on-success [::save-search-results]
              :on-failure [::failure]}})))

(defn sanitize-filter-term [term]
  (str/replace term #"[ ()]" " "))

(defn build-url-for-multi-filter-search
  "Builds a Typesense search URL with the given base URI, filters, and search term.

  Arguments:
  - `base-uri` (string): The base URL of the Typesense API (e.g., http://localhost:8108).
  - `filters` (map): A map of filters where keys are filter attributes (keywords)
     and values are collections of maps, each containing an `:id` key.
     Example:
     {:about [{:id \"https://example.org/1\"} {:id \"https://example.org/2\"}]
      :type [{:id \"https://example.org/3\"}]}
  - `search-term` (string): The term to search for, defaults to `*` for wildcard searches.

  Returns:
  - (string): A fully constructed URL for querying the Typesense API."
  [base-uri filters search-term]
  (let [extract-ids (fn [filter-key]
                      (->> (get filters filter-key)
                           (map :id)
                           (str/join ",")))
        filter-by (->> filters
                       (filter (fn [[_ v]] (seq v))) ; Exclude empty values
                       (map (fn [[filter-key _]]
                              (let [ids (extract-ids filter-key)]
                                (str (name filter-key) ".id" ":=[" ids "]"))))
                       (str/join "&&"))]
    (str base-uri
         "?q=" (or search-term "*")
         "&query_by=name,about,description,creator"
         (when (seq filter-by)
           (str "&filter_by=" filter-by)))))

(re-frame/reg-event-fx
 ::handle-multi-filter-search
 (fn [cofx [_ [filters search-term]]]
   (let [_ (.log js/console (clj->js filters))
         uri (build-url-for-multi-filter-search (str config/typesense-uri "search")
                                                filters
                                                search-term)
         _ (.log js/console "uri" uri)]
     {:fetch {:method :get
              :url uri
              :headers {"x-typesense-api-key" "xyz"}
              :mode :cors
              :credentials :omit
              :timeout 5000
              :response-content-types {#"application/.*json" :json}
              :on-success [::save-search-results]
              :on-failure [::failure]}})))

(comment
  "http://localhost:8108/collections/amb/documents//collections/amb/documents/search?q=chemie&query_by=name,about,description,creator"
  "http://localhost:8108/collections/amb/documents/search?q=biologie&query_by=name,about,description,creator&filter_by=about.id:=[https://w3id.org/kim/hochschulfaechersystematik/n42]&&learningResourceType.id:=[]")

(re-frame/reg-event-fx
 ::handle-filter-search
 (fn [cofx [_ [filter-attribute filter-term search-term]]]
   (let [uri (str config/typesense-uri
                  "search?q="
                  (or search-term "*")
                  "&query_by="
                  "name,about,description,creator"
                  (when filter-attribute
                    (str
                     "&filter_by="
                     filter-attribute
                     ":="
                     (sanitize-filter-term filter-term))))] ;; parantetheses seem to cause error when filtering
     {:fetch {:method :get
              :url uri
              :mode :cors
              :credentials :omit
              :headers {"x-typesense-api-key" "xyz"}
              :timeout 5000
              :response-content-types {#"application/.*json" :json}
              :on-success [::save-search-results]
              :on-failure [::failure]}})))

(re-frame/reg-event-db
 ::save-search-results
 (fn [db [_ results]]
   (let [raw-result-events (map #(-> % :document :event_raw) (-> results :body :hits))]
     (-> db
         (assoc  :search-results (-> results :body :hits))
         (update :events into raw-result-events)))))

(re-frame/reg-event-fx
 ::load-profile
 (fn [cofx [_ pubkey]]
   (let [sockets (filter #(= "connected" (:status %)) (-> cofx :db :sockets))
         profile-request ["REQ"
                          (make-sub-id "profile" pubkey)
                          {:kinds [0]
                           :authors [pubkey]
                           :limit 10}]]
     {::request-from-relay [sockets profile-request]})))

(re-frame/reg-event-db
 ::set-md-scheme
 (fn [db [_ md-scheme]]
   (assoc db :selected-md-scheme md-scheme)))

