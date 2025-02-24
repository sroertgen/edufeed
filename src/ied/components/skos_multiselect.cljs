(ns ied.components.skos-multiselect
  (:require [re-frame.core :as re-frame]
            [reagent.core :as reagent]
            [clojure.string :as str]
            [ied.components.icons :as icons]
            [ied.subs :as subs]
            [ied.events :as events]))

(defn concept-checkbox [concept field toggled disable-on-change]
  [:input {:type "checkbox"
           :checked (or toggled false)
           :class "checkbox checkbox-warning"
           :on-change (fn [] (when-not disable-on-change (re-frame/dispatch [::events/toggle-concept [concept field]])))}])

(defn highlight-match
  "Wraps the matching part of the text in bold markers, preserving the original capitalization.
   Arguments:
   - `text`: The full text (string).
   - `query`: The search term (string).
   Returns:
   - A Hiccup structure with the matching part bolded, or the original text if no match."
  [text query]
  (if (and text query)
    (let [lower-text (str/lower-case text)
          lower-query (str/lower-case query)
          index (str/index-of lower-text lower-query)]
      (if index
        [:span
         (subs text 0 index)
         [:span {:class "font-bold"} (subs text index (+ index (count query)))]
         (subs text (+ index (count query)))]
        text))
    text))

(defn concept-label-component
  [[concept field search-input]]
  (fn []
    (let [toggled-concepts @(re-frame/subscribe [::subs/toggled-concepts])
          toggled (some #(= (:id %) (:id concept)) toggled-concepts) ;; TODO could also be a subscription?
          prefLabel (highlight-match (-> concept :prefLabel :de) search-input)]
      [:li
       (if-let [narrower (:narrower concept)]
         [:details {:open (or search-input false)} ;; open on search
          [:summary {:class (when toggled "bg-orange-400 text-black")}
           [concept-checkbox concept field toggled]
           prefLabel]
          [:ul {:tabIndex "0"}
           (for [child narrower]
             ^{:key (:id child)} [concept-label-component [child field search-input]])]]
         [:a {:class (when toggled "bg-orange-400 text-black")
              :on-click (fn [] (re-frame/dispatch [::events/toggle-concept [concept field]]))}
          [concept-checkbox concept field toggled true]
          [:p
           prefLabel]])])))

(defn get-nested
  "Retrieve all values from nested paths in a node.
   Arguments:
   - `node`: The map representing a node.
   - `paths`: A sequence of keyword paths to retrieve values from the node."
  [node paths]
  (map #(get-in node %) paths))

(defn match-node?
  "Checks if the query matches any field value in the provided paths.
   Arguments:
   - `node`: The map representing a node.
   - `query`: The search term (string).
   - `paths`: A sequence of keyword paths to check in the node."
  [node query paths]
  (some #(str/includes? (str/lower-case (str %)) (str/lower-case  query)) (get-nested node paths)))

(defn search-concepts
  "Recursively search for matches in `hasTopConcept` and `narrower`.
   Arguments:
   - `cs`: The Concept Scheme
   - `query`: The search term (string).
   - `paths`: A sequence of keyword paths to check in each concept node.
   Returns:
   - A map with the same structure as `cs`, containing only matching concepts
     and their parents."
  [cs query paths]
  (letfn [(traverse [concept]
            (let [children (:narrower concept)
                  matched-children (keep traverse children)
                  node-matches (match-node? concept query paths)]
              (when (or node-matches (seq matched-children))
                (-> concept
                    (assoc :narrower (when (seq matched-children) matched-children))))))]
    (if-let [filtered-concepts (keep traverse (:hasTopConcept cs))]
      (assoc cs :hasTopConcept filtered-concepts)
      nil)))

(comment
  (def data {:id "http://w3id.org/openeduhub/vocabs/locationType/"
             :type "ConceptScheme"
             :title {:de "Typ eines Ortes, einer Einrichtung oder Organisation"
                     :en "Type of place, institution or organisation"}
             :hasTopConcept [{:id "http://w3id.org/openeduhub/vocabs/locationType/bdc2d9f1-bef6-4bf4-844d-4efc1c99ddbe"
                              :prefLabel {:de "Lernort"}
                              :narrower [{:id "http://w3id.org/openeduhub/vocabs/locationType/0d08a1e9-09d4-4024-9b5c-7e4028e28ce5"
                                          :prefLabel {:de "Kindergarten/-betreuung"}}
                                         {:id "http://w3id.org/openeduhub/vocabs/locationType/1d0965c1-4a3e-4228-a600-bfa1d267e4d9"
                                          :prefLabel {:de "Schule"}}
                                         {:id "http://w3id.org/openeduhub/vocabs/locationType/729b6724-1dd6-476e-b871-44383ac11ef3"
                                          :prefLabel {:de "berufsbildende Schule"}}]}
                             {:id "http://test.com/"
                              :prefLabel {:de "Test"}}]})

  (def fields [[:prefLabel :de] [:prefLabel :en]])
  (def query "Test")
  (println (pr-str (search-concepts data query fields))))

(defn skos-multiselect-component
  [[scheme field field-title]]
  (let [search-input (reagent/atom "")
        cs (re-frame/subscribe
            [::subs/concept-scheme scheme])]
    (re-frame/dispatch
     [::events/fetch-missing-concept-scheme scheme])
    (fn []
      (let [filtered-cs (search-concepts @cs @search-input [[:prefLabel :de]])] ;; TODO needs to be otherwise configured for multi language stuff
        [:div
         {:class "dropdown w-full"
          :key (:id @cs)}
         [:div
          {:tabIndex "0"
           :role "button"
           :class "btn m-1 grow w-full"}
          field-title]
         [:div {:class "dropdown-content z-[1]"}
          [:ul {:class "menu bg-base-200 rounded-box w-96 "
                :tabIndex "0"}
           [:label {:class "input flex items-center gap-2"}
            [:input {:class "grow"
                     :placeholder "Suche..."
                     :type "text"
                     :on-change (fn [e]
                                  (reset! search-input (-> e .-target .-value)))}]
            [icons/looking-glass]]
           (doall
            (for [concept (:hasTopConcept filtered-cs)]
              ^{:key (str @search-input (:id concept))} [concept-label-component [concept field @search-input]]))]]]))))

