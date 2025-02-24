(ns ied.views.search
  (:require    [re-frame.core :as re-frame]
               [ied.routes :as routes]
               [ied.subs :as subs]
               [ied.events :as events]
               [reagent.core :as reagent]
               [clojure.string :as str]
               [ied.components.resource :as resource-components]
               [ied.components.skos-multiselect :refer [skos-multiselect-component]]
               [ied.components.icons :as icons]
               [ied.nostr :as nostr]
               [cljs.core :as c]))

;; TODO refactor this to also work with raw event data
(defn extract-string [result]
  (let [document (:document result)
        event-info (select-keys document [:id :event_created_at :event_kind :event_pubkey])
        extracted-string (map (fn [k]
                                {:keyword k
                                 :event-info event-info})
                              (get document :keywords))]
    extracted-string))

(defn group-by-string [data]
  (let [extracted-string (into [] (apply concat (map extract-string data)))
        grouped-keyword (group-by :keyword extracted-string)]
    grouped-keyword))

(defn result [r]
  ; r is an array of matching event results 
  (let [id (nostr/get-d-id-from-event (-> (first r) :document :event_raw))
        pubkeys (map #(get-in % [:document :event_pubkey]) r)
        profiles (re-frame/subscribe [::subs/profiles pubkeys])
        name (first (map #(get-in % [:document :name]) r))
        description (first (map #(get-in % [:document :description]) r))
        events (map #(-> % :document :event_raw) r)
        keywords (group-by-string r)
        latest-event (-> (last (sort-by #(-> r :document :event_created_at) r))
                         :document
                         :event_raw)
        naddr (nostr/naddr-from-event latest-event)]
    (fn []
      [:div {:class "flex flex-row"}
       [:div {:class "flex flex-col w-3/4"}
        [:div {:class "flex flex-row items-center mt-2"}
         [:div
          {:class "avatar-group -space-x-6 "}
          (doall
           (for [[pubkey profile] @profiles]
             ^{:key pubkey} [:div
                             {:class "avatar bg-neutral"}
                             [:div
                              {:class "w-8"}
                              [:img
                               {:src
                                (nostr/profile-picture profile pubkey)}]]]))]
         [:p (str
              (str/join
               ", "
               (remove str/blank? (map  (fn [[_ p]] (get p :display_name "unbekannt")) @profiles)))
              (if (> (count @profiles) 1)
                " haben"
                " hat")
              " das geteilt.")]]

        [:a {:href id
             :target "_blank"
             :class "text-xl hover:underline"} name]
        [:div
         (resource-components/skos-tags [events "about"])]
              ;; keywords
        [:div {:class "flex flex-wrap"}
         (doall
          (for [[k v] keywords]
            ^{:key k} (resource-components/keywords-component k)))]

        [:p {:class "line-clamp-3"} description]

        [:div {:class "flex flex-row mb-0 mt-auto justify-between"}
         [:label {:class "btn  "
                  :on-click #(re-frame/dispatch [::events/navigate [:naddr-view :naddr naddr]])}
          "Details"]
         [resource-components/add-to-list latest-event]]]

       [:div {:class "w-1/4"}
        [:figure
         [:img
          {:class "h-48 object-cover mx-auto m-2"
           :loading "lazy"
           :src
           (nostr/get-image-from-metadata-event latest-event)
           :alt ""}]]]])))

; (defn filter-bar []
;   (let [concept-schemes (re-frame/subscribe
;                           [::subs/concept-schemes ["https://w3id.org/kim/hochschulfaechersystematik/scheme"]])
;         _ (doall (for [[cs-uri _] (filter (fn [[_ v]] (nil? v)) @concept-schemes)]
;                    (re-frame/dispatch [::events/skos-concept-scheme-from-uri  cs-uri])))]

;     (fn []
;     [:div
;      (doall
;       (for [cs (keys @concept-schemes)]
;         ^{:key cs} [skos-multiselect-component [(get @concept-schemes "https://w3id.org/kim/hochschulfaechersystematik/scheme")
;                                                 :about
;                                                 "Fachsystematik"]]))]))) ;; TODO reuse skos components

;; TODO make filters configurable with a map

(def filters
  [{:scheme "https://w3id.org/kim/hochschulfaechersystematik/scheme"
    :field :about
    :title "Fachsystematik"}
   {:scheme "https://w3id.org/kim/hcrt/scheme"
    :field :learningResourceType
    :title "Typ"}])

(defn filter-bar []
  (fn []
    (let [concept-schemes (re-frame/subscribe
                           [::subs/concept-schemes (map :scheme filters)])]
      ;; FIXME put this in multiselect component?
      (re-frame/dispatch
       [::events/fetch-missing-concept-scheme
        (->> @concept-schemes
             (filter (fn [[_ v]] (nil? v)))
             (map first))])

      [:div {:class "flex flex-row gap-2"}
       (doall
        (for [filter filters]
          ^{:key  (:scheme filter)}
          [skos-multiselect-component [(:scheme filter)
                                       (:field filter)
                                       (:title filter)]]))])))

(defn has-any-values?
  "checks if a map has any values"
  [m]
  (some
   (fn [[_ v]]
     (cond
       (map? v) (has-any-values? v)
       (coll? v) (seq (filter (complement nil?) v))
       :else (not (nil? v))))
   m))

(defn active-filters []
  (let [active-filters (re-frame/subscribe [::subs/active-filters])]
    (when (has-any-values?  @active-filters)
      [:div {:class "my-2 flex flex-nowrap gap-2"}
       (doall
        (for [field (keys (filter (fn [[_ v]] (seq v)) @active-filters))]
          (for [concept (field @active-filters)]
            ^{:key (:id concept)} [:div {:class "flex flex-row badge badge-lg badge-secondary p-3 gap-1 cursor-default"}
                                   [:span {:class ""} (-> concept :prefLabel :de)]
                                   [:span {:on-click (fn [] (re-frame/dispatch [::events/toggle-concept [concept field]]))
                                           :class "cursor-pointer"}
                                    [icons/close-icon]]])))])))

(defn search-bar []
  (let [search-term (reagent/atom nil)
        filters (re-frame/subscribe [::subs/active-filters])
        show-filter-bar (reagent/atom false)]
    (fn  []
      [:div {:class "flex flex-col w-3/4"}
       [:div {:class "flex"} ;;TODO fix layout
        [:form {:class "w-full"
                :on-submit (fn [e]
                             (.preventDefault e)
                             (re-frame/dispatch [::events/handle-multi-filter-search [@filters @search-term]]))}
         [:label
          {:class "input input-bordered flex items-center gap-2 w-1/2 mx-auto"}
          [:input {:type "search"
                   :class "grow"
                   :placeholder "Search"
                   :on-change (fn [e] (reset! search-term (-> e .-target .-value)))}]
          [:svg
           {:xmlns "http://www.w3.org/2000/svg",
            :viewBox "0 0 16 16",
            :fill "currentColor",
            :class "h-4 w-4 opacity-70"}
           [:path
            {:fill-rule "evenodd",
             :d
             "M9.965 11.026a5 5 0 1 1 1.06-1.06l2.755 2.754a.75.75 0 1 1-1.06 1.06l-2.755-2.754ZM10.5 7a3.5 3.5 0 1 1-7 0 3.5 3.5 0 0 1 7 0Z",
             :clip-rule "evenodd"}]]]]
        [:button {:on-click #(reset! show-filter-bar (not @show-filter-bar))
                  :class "btn"} [icons/filter-icon] "Filter"]]
       [:div
        [active-filters]
        (when @show-filter-bar
          [filter-bar])]])))

(defn search-view-panel []
  (let [grouped-results (re-frame/subscribe [::subs/grouped-search-results])]
    [:div {:class ""}
     [search-bar]
     (when (not-empty @grouped-results)
       [:div {:class "divide-y divide-slate-700 flex flex-col gap-2"}
        (doall
         (for [[k v] @grouped-results]
           ^{:key k} [result v]))])]))

(defmethod routes/panels :search-view-panel [] [search-view-panel])

