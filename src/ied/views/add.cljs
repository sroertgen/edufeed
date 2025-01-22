(ns ied.views.add
  (:require [re-frame.core :as re-frame]
            [clojure.string :as str]
            [reagent.core :as reagent]
            [ied.subs :as subs]
            [ied.events :as events]
            [ied.components.icons :as icons]
            [ied.routes :as routes]
            [ied.components.skos-multiselect :refer [skos-multiselect-component]]))

(def md-scheme-map
  {:amblight {:name {:title "Name"
                     :type :string}
              :uri {:title "URL"
                    :type :string}
              :author {:title "Author"
                       :type :string}
              :keywords {:title "Schlagworte"
                         :type :array
                         :items {:type :string}}
              :about {:title "Worum geht es??"
                      :type :skos
                      :schemes ["https://w3id.org/kim/hochschulfaechersystematik/scheme"]}
              :learningResourceType {:type :skos
                                     :schemes ["https://w3id.org/kim/hcrt/scheme"]}}
   :genmint (array-map :name {:title "Name"
                              :type :string}
                       :uri {:title "URL"
                             :type :string}
                       :image {:title "Thumbnail"
                               :type :string}
                       :description {:title "Beschreibung"
                                     :type :string
                                     :x-string-type :textarea}
                       :creator {:title "Autor:innen"
                                 :type :array
                                 :items {:type :string}}
                       :publisher {:title "Veröffentlicher:innen"
                                   :type :array
                                   :items {:type :string}}
                       :funder {:title "Gefördert durch"
                                :type :array
                                :items {:type :string}}
                       :datePublished {:title "Publikationsdatum"
                                       :type :string
                                       :format :date}
                       :learningResourceType {:type :skos
                                              :schemes ["https://w3id.org/kim/hcrt/scheme"]}
                       :teaches {:title "Welche Kompetenzen werden adressiert?"
                                 :type :skos
                                 :schemes [""]}
                       :about {:title "Fächersystematik"
                               :type :skos
                               :schemes ["https://w3id.org/kim/hochschulfaechersystematik/scheme"]}
                       :keywords {:title "Schlagworte"
                                  :type :array
                                  :items {:type :string}}
                       :inLanguage {:title "Sprache"
                                    :type :string
                                    :enum ["de" "en"]}
                       :isAccessibleForFree {:title "Frei zugänglich"
                                             :type :string
                                             :enum ["ja" "nein"]}
                      ;; LICENSE
                       :license {:title "Lizenz"
                                 :type :string
                                 :enum ["CC-0" "CC-BY"]}
                      ;; ConditionsOfAccess
                      ;; audience
                     ;; didaktische Methoden und Planungshilfen
                    ;;  educationalLevel 
                       )
   :amb {:name {:title "Name"
                :type :string}
         :uri {:title "URL"
               :type :string}
         :author {:title "Author"
                  :type :string}
         :learningResourceType {:title "Learning Resource Typ"
                                :type :skos
                                :schemes ["https://w3id.org/kim/hcrt/scheme"]}
         :about {:type :skos
                 :schemes ["https://w3id.org/kim/hochschulfaechersystematik/scheme"]}}})

(defn array-fields-component [selected-md-scheme field field-title]
  (let [array-items-type (get-in selected-md-scheme [field :items :type])
        fields (re-frame/subscribe [::subs/md-form-array-input-fields field])]
    (fn []
      [:div {:class "flex flex-col gap-1"}
       [:p field-title]
       (case array-items-type
         :string (doall (for [[k v] @fields]
                          [:div {:class "flex flex-row gap-1"
                                 :key k}
                           [:input {:type "text"
                                    :class "input input-bordered w-full"
                                    :default-value ""
                                    :on-blur (fn [e]
                                               (re-frame/dispatch
                                                [::events/handle-md-form-array-input
                                                 [field
                                                  k
                                                  (-> e .-target .-value)]]))}]
                           [:button {:class "btn btn-square"
                                     :on-click
                                     #(re-frame/dispatch
                                       [::events/handle-md-form-rm-input
                                        [field k]])}  [icons/close-icon]]])))
       [:button {:class "btn"
                 :on-click #(re-frame/dispatch
                             [::events/handle-md-form-add-input [field]])}
        "Add field"]])))

;; add resource form
(defn add-resource-form
  []
  (let [md-scheme (re-frame/subscribe [::subs/selected-md-scheme])
        md-form-image (re-frame/subscribe [::subs/md-form-image])]
    (fn []
      [:form  {:class "flex flex-col space-y-4"
               :on-submit (fn [e]
                            (.preventDefault e))}
       [:select
        {:default-value @md-scheme
         :on-change (fn [e]
                      (let [value (-> e .-target .-value keyword)]
                        (re-frame/dispatch [::events/set-md-scheme value])))
         :class "select select-bordered w-full max-w-xs"}
        [:option {:disabled true} "Select a Metadata Scheme"]
        [:option {:value "amblight"} "AMB Light"]
        [:option {:value "genmint"} "GenMint"]]

       (let [selected-md-scheme (get md-scheme-map @md-scheme (:amblight md-scheme-map))]
         [:div {:class "flex flex-col gap-2"}
          (when @md-form-image
            [:img {:src @md-form-image}])
          (doall
           (for [field (keys selected-md-scheme)]
             (let [field-type (-> selected-md-scheme field :type)
                   field-title (get-in selected-md-scheme [field :title] (name field))]
               (cond ;; TODO the conditions need to get more logic to evaluate form fields in order
                 (and (= :string field-type)
                      (= :textarea (-> selected-md-scheme field :x-string-type)))
                 [:textarea {:key field
                             :type ""
                             :class "textarea textarea-bordered grow"
                             :placeholder field-title
                             :on-blur #(re-frame/dispatch [::events/handle-md-form-input [field (-> % .-target .-value)]])}]
                 (and (= :string field-type)
                      (-> selected-md-scheme field :enum))
                 (let [enum-vals (-> selected-md-scheme field :enum)]
                   [:select
                    {:key field
                     :class "select select-bordered w-full"}
                    [:option {:disabled "" :selected ""} field-title]
                    (doall
                     (for [val enum-vals]
                       [:option {:key val} val]))])
                 (and (= :string field-type)
                      (= :date (-> selected-md-scheme field :format)))
                 [:div {:class "my-4"}
                  [:label {:for field} field-title]
                  [:input
                   {:type "date"
                    :on-blur #(re-frame/dispatch [::events/handle-md-form-input [field (-> % .-target .-value)]])
                    :id field
                    :name field-title
                    ; :value "2018-07-22"
                    :min "2018-01-01"
                    :max "2018-12-31"}]]
                 (= :string field-type)
                 [:label {:key field
                          :class "input input-bordered flex items-center gap-2"}
                  (-> selected-md-scheme field :title)
                  [:input {:type "text"
                           :class "grow"
                           :placeholder (name field)
                           :on-blur #(re-frame/dispatch [::events/handle-md-form-input [field (-> % .-target .-value)]])}]] ;; TODO maybe better handle this with an atom in the component
                 (= :skos field-type)
                 (let [concept-schemes @(re-frame/subscribe
                                         [::subs/concept-schemes (-> selected-md-scheme field :schemes)])
                       _ (doall (for [[cs-uri _] (filter (fn [[_ v]] (nil? v)) concept-schemes)]
                                  (re-frame/dispatch [::events/skos-concept-scheme-from-uri  cs-uri])))]

                   (doall
                    (for [cs (keys concept-schemes)]
                      ^{:key cs} [skos-multiselect-component [(get concept-schemes cs)
                                                              field
                                                              field-title]])))
                 (= :array field-type)
                 ^{:key field} [array-fields-component selected-md-scheme field field-title]
                 :else
                 [:p {:key field} "field type not found"]))))])

       [:button {:class "btn btn-warning w-1/2 mx-auto"
                 :on-click #(do
                              (re-frame/dispatch [::events/publish-resource])
                              (re-frame/dispatch [::events/navigate [:home]]))}
        "Publish Resource"]])))

(defn add-resource-by-json []
  (let [s (reagent/atom {:json-string ""})]
    (fn []
      [:div {:class "flex flex-col"}
       [:p "Paste an AMB object"]
       [:form {:class "flex flex-col"
               :on-submit (fn [e] (.preventDefault e))}
        [:textarea {:rows 10
                    :on-change (fn [e]
                                 (swap! s assoc :json-string (-> e .-target .-value)))}]
        [:button {:class "btn btn-warning"
                  :on-click #(do
                               (re-frame/dispatch [::events/convert-amb-string-and-publish-as-nostr-event (:json-string @s)])
                               (re-frame/dispatch [::events/navigate [:home]]))}
         "Publish as Nostr Event"]]])))

;; TODO try again using xhrio
(defn add-resosurce-by-uri []
  (let [uri (reagent/atom {:uri ""})]
    (fn []
      [:form {:class "flex flex-col"
              :on-submit (fn [e] (.preventDefault e))}
       [:label {:for "uri"} "URI: "]
       [:input {:id "uri"
                :on-change (fn [e]
                             (swap! uri assoc :uri (-> e .-target .-value)))}]
       [:button {:class "btn btn-warning"
                 :on-click #(re-frame/dispatch [::events/publish-amb-uri-as-nostr-event (:uri @uri)])}
        "Publish as Nostr Event"]])))

;; Add Resource Panel
(defn add-resource-panel []
  [:div
   {:class "sm:w-1/2 p-2 mx-auto flex flex-col space-y-4"}
   [add-resource-form]
   [:details
    {:tabIndex "0",
     :class "collapse collapse-arrow border-base-300 bg-base-200 border"}
    [:summary
     {:class "collapse-title text-xl font-medium"}
     "Advanced Options"]
    [:div
     {:class "collapse-content"}
     [add-resource-by-json]
     [add-resosurce-by-uri]]]])

(defmethod routes/panels :add-resource-panel [] [add-resource-panel])
