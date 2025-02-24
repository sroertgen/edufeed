(ns ied.routes
  (:require
   [bidi.bidi :as bidi]
   [pushy.core :as pushy]
   [re-frame.core :as re-frame]
   [ied.events :as events]))

(defmulti panels identity)
(defmethod panels :default [] [:div "No panel found for this route."])

(def routes
  (atom
   ["/"
    {""      :home
     "search" :search-view
     "add-resource" :add-resource
     "keys" :keys
     "feed" :event-feed
     "about" :about
     "settings" :settings
     ["" [#"npub1[ac-hj-np-z02-9]{58}" :npub]] {"" :npub-view
                                                "/" :npub-view}
     ["" [#"naddr1[ac-hj-np-z02-9]+" :naddr]] {"" :naddr-view
                                               "/" :naddr-view}}]))

(comment
  (navigate! (parse "/npub1r30l8j4vmppvq8w23umcyvd3vct4zmfpfkn4c7h2h057rmlfcrmq9xt9ma/"))
  (navigate! (parse "/naddr1r30l8j4vmppvq8w23umcyvd3vct4zmfpfkn4c7h2h057rmlfcrmq9xt9ma/"))
  (apply url-for [:npub-view :npub "npub1r30l8j4vmppvq8w23umcyvd3vct4zmfpfkn4c7h2h057rmlfcrmq9xt9ma"])
  (apply url-for [:naddr-view :naddr "naddr1r30l8j4vmppvq8w23umcyvd3vct4zmfpfkn4c7h2h057rmlfcrmq9xt9ma"])
  (apply url-for [:home]))

(defn parse
  [url]
  (bidi/match-route @routes url))

(defn url-for
  [& args]
  (apply bidi/path-for (into [@routes] args)))

(defn dispatch
  [route]
  (let [panel (keyword (str (name (:handler route)) "-panel"))]
    (re-frame/dispatch  [::events/set-route {:route route :panel panel}])))

(defonce history
  (pushy/pushy dispatch parse))

(defn navigate!
  [handler]
  (pushy/set-token! history (apply url-for handler)))

(defn start!
  []
  (pushy/start! history))

(re-frame/reg-fx
 :navigate
 (fn [handler]
   (navigate! handler)))

