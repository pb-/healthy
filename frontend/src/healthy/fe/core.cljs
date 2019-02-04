(ns ^:figwheel-hooks healthy.fe.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [secretary.core :as secretary :refer-macros [defroute]]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]))

(def state (r/atom {:loading true}))

(defn home []
  [:p "Welcome to agile health check!"])

(defn make-select [s i]
  (fn []
    (swap! state assoc :selected
           (if (= (:selected s) i)
             nil
             i))))

(defn survey [s]
  [:div
   [:h1 (str
          (get-in s [:survey :template :title])
          " – "
          (:name ((get-in s [:survey :template :dimensions]) 0)))]
   [:div#panel
    (concat (for [index (range 5)]
              ^{:key index} [:p
                             {:class (str "score" (when (or (not (:selected s)) (= (:selected s) index)) (str " s" index)))
                              :on-click (make-select s index)} (inc index)])
            (for [index (range 5)]
              ^{:key (+ index 10)}
              [:div
               [:p.description
                {:on-click (make-select s index)}
                (:description ((:options ((get-in s [:survey :template :dimensions]) 0)) index))]
               (when (= index (:selected s))
                 [:div
                  [:button "Continue"]
                  [:textarea.comment
                   {:placeholder "Optional comment"
                    :on-change #(swap! state assoc :comment (-> % .-target .-value))
                    :value (:comment s)}]])]))]])

(defn loading-display []
  [:p "Loading..."])

(defn root []
  (let [s @state]
    (if (:loading s)
      (loading-display)
      (case (:screen s)
        :take-survey (survey s)
        (home)))))

(defn main []
  (r/render [root] (.getElementById js/document "app")))

(main)

(defroute "/survey/:id" {:as params}
  (go
    (let [response (<! (http/get
                         (str "http://localhost:8080/api/query/survey/" (:id params))
                         {:with-credentials? false}))]
      (swap! state merge
             {:loading false
              :screen :take-survey
              :survey-id (:id params)
              :survey (:body response)}))))

(defroute "*" []
  (swap! state assoc :loading false))

(secretary/dispatch! (subs js/location.hash 1))
