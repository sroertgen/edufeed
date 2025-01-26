(ns ied.views
  (:require
   [re-frame.core :as re-frame]
   [cljs.pprint :refer [pprint]]
   [ied.events :as events]
   [ied.routes :as routes]
   [ied.subs :as subs]
   [ied.nostr :as nostr]
   [ied.components.icons :as icons]
   [ied.opencard.views :as opencard]
   [ied.views.search :as search]
   [ied.views.resource :as resource]
   [ied.views.relay-settings :as rs]
   [ied.components.modals :as modals]
   [ied.views.add :as add]
   [ied.components.resource :as resource-component]
   [reagent.core :as reagent]))

;; event data modal
(defn event-data-modal []
  (let [visible? @(re-frame/subscribe [::subs/show-event-data-modal])
        selected-event @(re-frame/subscribe [::subs/selected-event])]
    (when visible?
      [:dialog {:open visible? :class "modal"}
       [:div {:class "modal-box relative flex flex-col"}
        [:h3 {:class "text-lg font-bold"} (nostr/get-name-from-metadata-event selected-event)]
        [:pre (with-out-str (pprint selected-event))]
        [:p {:class "py-4"} "Press ESC key or click outside to close"]]

       [:form {:on-click #(re-frame/dispatch [::events/toggle-show-event-data-modal])
               :method "dialog" :class "modal-backdrop"}
        [:button "close"]]])))

;; metadata event component
;; TODO add author profile name and image
(defn metadata-event-component [event]
  (let [selected-events @(re-frame/subscribe  [::subs/selected-events])]
    [:div {:class "flex flex-row gap-2"}
     [:div {:class "w-3/4 p-2 min-h-full flex flex-col mt-2 justify-between"}
      [:div {:class ""}
       [:div {:class "flex flex-row gap-2 "}
        [:div {:class "w-6 h-6 rounded-full bg-white"}
         (cond
           (= 30004 (:kind event)) [icons/layer-icon]
           (= 30142 (:kind event)) [icons/file-icon])]
        [:a {:href (nostr/get-d-id-from-event event)
             :class "font-bold hover:underline text-ellipsis overflow-hidden"}
         (nostr/get-name-from-metadata-event event)]]
       [:div {:class "flex flex-wrap"}
        (resource-component/about-tags  [event])]
       [:p {:class "text-wrap"}
        (nostr/get-description-from-metadata-event event)]]

      [:div {:class "flex flex-row justify-between items-center"}
       [:button {:class "btn"
                 :on-click #(re-frame/dispatch [::events/toggle-show-event-data-modal event])} "Show Event Data"]
       [resource-component/add-to-list event]]]
     [:div {:class "w-1/4"}
      [:figure
       [:img
        {:class "h-48 object-cover mx-auto m-2"
         :loading "lazy"
         :src
         (nostr/get-image-from-metadata-event event)
         :alt ""}]]]]))

(defn activity-feed []
  (let [following @(re-frame/subscribe [::subs/following])
        npub @(re-frame/subscribe [::subs/npub])
        _ (when (nil? following)
            (when npub (re-frame/dispatch [::events/get-follow-set-for-npub npub])))
        following-events @(re-frame/subscribe [::subs/posts-from-pks-actor-follows])
        _ (.log js/console (count following-events) (< 5 (count following-events)))
        _ (when (> 5 (count following-events))
            (re-frame/dispatch [::events/events-from-pks-actor-follows following]))]

    [:div
     (doall
      (for [event following-events]
        [:p {:key (:id event)} (:content event)]))]))

;; events
(defn events-panel []
  (fn []
    (let [events @(re-frame/subscribe  [::subs/feed-events])
          selected-events @(re-frame/subscribe  [::subs/selected-events])]
      [:div {:class "flex flex-col mx-auto gap-4 "}
       [activity-feed]
       (if (> (count events) 0)
         [:div {:class "divide-y divide-slate-500"}
          (doall
           (for [event  events]
             [:div {:key (:id event)
                    :class ""}
              [metadata-event-component event]]))]

         [:p "no events there"])
       [:button {:class "btn"
                 :disabled (not (boolean (seq selected-events)))
                 :on-click #(re-frame/dispatch [::events/add-metadata-event-to-list [{:d "unique-id-1"
                                                                                      :name "Test List SC"}
                                                                                     selected-events]])}
        "Add To Lists"]])))

;; event feed component
(defn event-feed-component [event]
  (let [_ () #_(re-frame/dispatch [::events/add-confetti])]
    [:div
     {:class "animate-flyIn card bg-base-100 w-64 h-64 shadow-xl border border-white border-w "}
     (cond
       (= 30004 (:kind event)) [:p {:class "text-2xl float-right"} "📖"]
       (= 30142 (:kind event)) [:p {:class "text-2xl float-right"} "📚"])
     [:figure
      [:img
       {:class "h-48 object-contain"
        :src
        (nostr/get-image-from-metadata-event event)
        :alt ""}]]
     [:div
      {:class "card-body"}
      [:button {:on-click #(re-frame/dispatch [::events/toggle-show-event-data-modal event])} "Show Event Data"]]]))

(defn event-feed-panel []
  (let [events @(re-frame/subscribe  [::subs/feed-events])]
    [:div {:class ""}
     [:h1 "Event Feed"]
     [:p (str "Num of events: " (count events))]
     (if (> (count events) 0)
       [:div {:class "flex flex-col"}
        (doall
         (for [event events]
           [:div {:key (:id event)}
            [event-feed-component event]]))]
       [:p "no events there"])]))

(defmethod routes/panels :event-feed-panel [] [event-feed-panel])

;; shopping cart
(defn shopping-cart []
  (let [selected-events @(re-frame/subscribe [::subs/selected-events])]
    [:div {:class "dropdown dropdown-end"}
     [:div
      {:tabIndex "0", :role "button", :class "btn btn-ghost btn-circle"}
      [:div
       {:class "indicator"}
       [icons/shopping-cart]
       [:span {:class "badge badge-sm indicator-item"} (count selected-events)]]]
     [:div
      {:tabIndex "0",
       :class
       "card card-compact dropdown-content bg-base-100 z-[1] mt-3 w-52 shadow"}
      [:div
       {:class "card-body"}
       [:span {:class "text-lg font-bold"}
        (str (count selected-events)
             " Items")]
       [:div {:class "card-actions"}
        [:button {:class "btn"
                  :on-click #((modals/open-add-to-list-modal)

                              ; re-frame/dispatch [::events/toggle-show-lists-modal]
                              )}
         "Add To Lists"]]]]]))

(defn user-menu []
  (let [pk @(re-frame/subscribe [::subs/pk])
        user-profile (re-frame/subscribe [::subs/profile pk])]
    (if pk
      [:div
       {:class "dropdown dropdown-end"}
       [:div
        {:tabIndex "0",
         :role "button",
         :class "btn btn-ghost btn-circle avatar"}
        [:div
         {:class "w-10 rounded-full"}
         [:img
          {:alt "user image",
           :src
           (nostr/profile-picture @user-profile pk)}]]]
       [:ul
        {:tabIndex "0",
         :class
         "menu menu-sm dropdown-content bg-base-100 rounded-box z-[1] mt-3 w-52 p-2 shadow"}
        [:li
         [:a
          {:on-click #(re-frame/dispatch [::events/navigate [:npub-view :npub (nostr/get-npub-from-pk pk)]])}
          "Profile"]]
        [:li [:a {:on-click #(re-frame/dispatch [::events/navigate [:settings]])} "Settings"]]
        [:li [:a {:on-click #(re-frame/dispatch [::events/navigate [:keys]])} "Keys"]]
        [:li [:a {:on-click #(re-frame/dispatch [::events/logout])} "Logout"]]]]
      [:div
       {:class "dropdown dropdown-end"}
       [:div {:tabIndex "0", :role "button", :class "btn m-1"} "Login"]
       [:ul
        {:tabIndex "0",
         :class
         "dropdown-content menu bg-base-100 rounded-box z-[1] w-52 p-2 shadow"}
        [:li [:a {:on-click #(re-frame/dispatch [::events/create-sk])}
              "... Anonymously"]]
        [:li [:a {:on-click #(re-frame/dispatch [::events/login-with-extension])}
              "... with Extension"]]]])))

;; Header
(defn header []
  (let [pk @(re-frame/subscribe [::subs/pk])]
    [:div
     {:class "navbar sticky z-50 top-0 left-0 bg-base-100"}
     [:div
      {:class "flex-1"}
      [:a {:class "cursor-pointer text-xl font-bold"
           :on-click #(re-frame/dispatch [::events/navigate [:home]])}
       "edufeed"]
      #_[:a {:class "btn btn-circle"
             :on-click #(re-frame/dispatch [::events/navigate [:event-feed]])} "Event-Feed"]]
     [:div
      {:class "flex-none"}
      [:a {:disabled (nil? pk)
           :class "btn btn-circle btn-primary text-xl"
           :on-click #(re-frame/dispatch [::events/navigate [:add-resource]])} "+"]
      (when (not (nil? pk))
        [shopping-cart])
      [user-menu]]]))

;; Keys Panel
(defn keys-panel []
  (let [npub @(re-frame/subscribe [::subs/npub])
        pk (nostr/get-pk-from-npub npub)
        nsec @(re-frame/subscribe [::subs/nsec])] [:div
                                                   [:h1 "Keys"]
                                                   [:p (str "Your Npub: " npub)]
                                                   [:p (str "Your PK: " pk)]
                                                   [:p (str "Your Nsec: " nsec)]]))

(defmethod routes/panels :keys-panel [] [keys-panel])

;; Settings
(defn settings-panel []
  [:div {:class "w-3/4 mx-auto text-center"}
   [:h1 {:class "text-lg mb-2"} "Settings"]
   [rs/relays-panel]])

(defmethod routes/panels :settings-panel [] [settings-panel])

(defn sidepanel []
  (let [pk (re-frame/subscribe [::subs/pk])]
    (fn []
      [:div {:class "static flex flex-col m-2 "}
       [:ul {:class "menu rounded-box"}
        [:li
         [:a {:on-click #(re-frame/dispatch [::events/navigate [:home]])}
          "Feed"]]
        [:li [:a {:on-click #(re-frame/dispatch [::events/navigate [:search-view]])}
              "Search"]]
        #_[:li [:a  "My Network"]]
        (when @pk
          [:li  [:a {:on-click #(re-frame/dispatch [::events/navigate [:npub-view :npub (nostr/get-npub-from-pk @pk)]])}
                 "Bookmarks"]])
        #_[:li [:a {:on-click #(re-frame/dispatch [::events/navigate [:opencard-view]])} "Opencard"]]]])))

;; Home
(defn home-panel []
  (let [events (re-frame/subscribe [::subs/events])]
    [:div {:class "basis-5/6"}
     [events-panel]
     [:p (count @events)]]))

(defmethod routes/panels :home-panel [] [home-panel])

;; about
(defn about-panel []
  [:div
   [:h1 "This is the About Page."]
   [:div
    [:a {:on-click #(re-frame/dispatch [::events/navigate [:home]])}
     "go to Home Page"]]])

(defmethod routes/panels :about-panel [] [about-panel])

;; Lists
(defn list-component [list]
  (let [pk @(re-frame/subscribe [::subs/pk])
        ids-in-list (nostr/get-event-ids-from-list list)
        events-in-list @(re-frame/subscribe [::subs/events-in-list ids-in-list]) ;; TODO should be sorted after appeareande in tags
        ]
    [:div {:key (:id list)
           :class "p-2"}
     [:details
      {:class "collapse bg-base-200"}
      [:summary
       {:class "collapse-title text-xl font-medium"}
       (nostr/get-list-name list)]
      [:div {:class "collapse-content"}

       (doall
        (for [event events-in-list]
          [:div {:key  (:id event)}
           [metadata-event-component event]
           (when pk
             [:button {:class "btn btn-warning"
                       :on-click #(re-frame/dispatch [::events/delete-event-from-list [event list]])}
              "Delete Resource from List"])]))

       (when pk [:button {:class "btn btn-error"
                          :on-click #(re-frame/dispatch [::events/delete-list list])} "Delete List"])]]]))

;; npub / Profile
(defn npub-view-panel []
  (let [sockets @(re-frame/subscribe [::subs/sockets])
        route-params @(re-frame/subscribe [::subs/route-params])
        lists @(re-frame/subscribe [::subs/lists-for-npub])
        _ (when (not (seq lists)) (re-frame/dispatch [::events/get-lists-for-npub (:npub route-params)]))
        missing-events-from-lists @(re-frame/subscribe [::subs/missing-events-from-lists])
        _ (when (seq missing-events-from-lists) (re-frame/dispatch [::events/query-for-event-ids [sockets missing-events-from-lists]]))]
    [:div
     [:h1 (str "Hello Npub: " (:npub route-params))]
     (if-not (seq lists)
       [:p "No lists there...yet"]
       (doall
        (for [l lists]
          ^{:key (:id l)} [list-component l])))
     [:button {:class "btn ml-auto mr-0"
               :on-click #((modals/open-create-list-modal))}
      "Create New List"]]))

(defmethod routes/panels :npub-view-panel [] [npub-view-panel])

;; main
(defn main-panel []
  (let [active-panel (re-frame/subscribe [::subs/active-panel])]
    [:div {:class "relative"}
     [header]
     [:div {:class "flex flex-row"}
      [:div {:class "w-1/6"}
       [sidepanel]]
      [:div {:class "w-5/6 mt-1"}
       [:div {:class ""}
        (routes/panels @active-panel)]]
      [modals/add-to-lists-modal]
      [modals/create-list-modal]
      [event-data-modal]]]))

