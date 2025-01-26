(ns ied.views.relay-settings
  (:require [re-frame.core :as re-frame]
            [reagent.core :as reagent]
            [ied.subs :as subs]
            [ied.events :as events]
            [ied.components.icons :as icons]
            [ied.routes :as routes]))

;; relays
(defn add-relay-form
  [name uri]
  (let [s (reagent/atom {:name name
                         :uri uri})]
    (fn []
      [:form {:on-submit (fn [e]
                           (.preventDefault e)
                           )}
       [:div {:class "flex flex-row gap-2"}
        [:label  {:class "input input-bordered flex items-center gap-2"
                  :for name} "Name: "
         [:input {:class "grow"
                  :type :text
                  :name :name
                  :value (:name @s)
                  :on-change (fn [e]
                               (swap! s assoc :name (-> e .-target .-value)))}]]
        [:label {:class "input input-bordered flex items-center gap-2"
                 :for uri} "Uri: "
         [:input {:class "grow"
                  :type :text
                  :name :uri
                  :value (:uri @s)
                  :on-change (fn [e]
                               (swap! s assoc :uri (-> e .-target .-value)))}]]
        [:button {:class "btn"
                  :on-click #(re-frame/dispatch [::events/create-websocket {:name (:name @s)
                                                                            :id (random-uuid)
                                                                            :uri (:uri @s)}])}
         "Add Relay"]]])))

(defn relays-panel
  []
  (let [sockets (re-frame/subscribe [::subs/sockets])]
    [:div
     [add-relay-form]

     (if (> (count @sockets) 0)
       [:ul {:class "mt-2 flex flex-col gap-2"}
        (doall
         (for [socket @sockets]
           [:li {:key (:id socket)}
            [:div {:class "flex flex-row items-center gap-2 w-full"}
             [:div {:class "flex flex-row items-center w-1/4 gap-2"}
              [:span (:name socket)]
              [:div {:class "text-center"}
               (case (:status socket)
                 "connected" [icons/checkmark]
                 "error" [icons/close-icon]
                 [:span (:status socket)])]]
             [:button {:class "btn"
                       :disabled (not= "connected" (:status socket))
                       :on-click #(re-frame/dispatch [::events/load-events (:uri socket)])} "Load events"]
             (if (not= (:status socket) "connected")
               [:button {:class "btn"
                         :on-click #(re-frame/dispatch [::events/connect-to-websocket (:uri socket)])} "Connect"]
               [:button {:class "btn"
                         :on-click #(re-frame/dispatch [::events/close-connection-to-websocket (:uri socket)])} "Disconnect"])

             [:button {:class "btn btn-error"
                       :on-click #(re-frame/dispatch [::events/remove-websocket socket])} "Remove relay"]]]))]
       [:p "No relays found"]
       ;(re-frame/dispatch [::events/connect-to-default-relays])
       )]))

