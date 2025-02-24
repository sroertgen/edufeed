(ns ied.db
  (:require
   [ied.config :as config]))

(def default-db
  {:name "re-frame"
   :current-path nil
   :concept-schemes {}
   :confetti false
   :show-add-event false
   :events #{}
   :md-form-resource nil
   :selected-md-scheme nil
   :pk nil
   :sk nil
   :list-kinds [30001 30004]
   :follow-sets [30000]
   :resource-to-add nil
   :default-relays (concat
                    (if config/debug?
                      [{:name "relay-edu"
                        :id (random-uuid)
                        :uri "wss://relay-edu.edufeed.org"
                        :status "disconnected"}
                       {:name "strfry-1"
                        :uri "http://localhost:7777"
                        :id (random-uuid)
                        :status "disconnected"
                        :type ["outbox" "inbox"]}
                       {:name "strfry-2"
                        :uri "http://localhost:7778"
                        :id (random-uuid)
                        :status "disconnected"
                        :type ["outbox" "inbox"]}
                       {:name "rust-relay"
                        :uri "http://localhost:4445"
                        :id (random-uuid)
                        :status "disconnected"
                        :type ["outbox" "inbox"]}]
                      [{:name "SC24"
                        :uri "wss://relay.sc24.steffen-roertgen.de"
                        :id (random-uuid)
                        :status "disconnected"
                        :type ["outbox" "inbox"]}
                      {:name "Relay HED"
                        :uri "wss://relay-hed.edufeed.org"
                        :id (random-uuid)
                        :status "disconnected"
                        :type ["outbox" "inbox"]} 
                      {:name "Relay K12"
                        :uri "wss://relay-k12.edufeed.org"
                        :id (random-uuid)
                        :status "disconnected"
                        :type ["outbox" "inbox"]}
                       ])
                    [{:name "Purplepages"
                      :uri "wss://purplepag.es"
                      :id (random-uuid)
                      :status "disconnected"
                      :type ["search"]}])
   :selected-events #{}
   :selected-list-ids #{}
   :show-event-data-modal false
   :sockets []
   :search-results nil
   :user-language "de"})

(comment
  (filter
   (fn [s]
     (some
      #(= "search" %)
      (:type s)))
   (-> default-db :default-relays)))
