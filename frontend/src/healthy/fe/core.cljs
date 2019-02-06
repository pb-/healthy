(ns ^:figwheel-hooks healthy.fe.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [secretary.core :as secretary :refer-macros [defroute]]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]))

(def endpoint "http://localhost:8080")
(defonce state (r/atom {:loading true}))

(defn home []
  [:p "Welcome to agile health check!"])

(defn make-select [s i]
  (fn []
    (swap! state assoc :selected
           (if (= (:selected s) i)
             nil
             i))))

(defn grade [survey-id dimension-id score comment]
  (go (let [response (<! (http/post
                           (str endpoint "/api/command")
                           {:with-credentials? false
                            :edn-params {:type :dimension-graded
                                         :user-id "0123401234"
                                         :survey-id survey-id
                                         :dimension-id dimension-id
                                         :score score
                                         :comment comment}}))]
        (if (= (:status response) 201)
          (swap! state update-in [:dimension] inc)
          (swap! state assoc :error? true)))))

(defn all-done [s]
  [:div.center
   [:h1 "All done!"]
   [:p.emoji "🎉"]
   [:p "Thank you for your feedback."]])

(defn survey-step [s]
  [:div
   [:h1 (str
          (get-in s [:survey :template :title])
          " – "
          (:name ((get-in s [:survey :template :dimensions]) (:dimension s))))]
   [:div#panel
    (concat (for [index (range 5)]
              ^{:key index} [:p
                             {:class (str "score" (when (or (not (:selected s)) (= (:selected s) index)) (str " s" index)))
                              :on-click (make-select s index)} (inc index)])
            (for [index (range 5)]
              (let [dimension ((get-in s [:survey :template :dimensions]) 0)
                    option ((:options dimension) index)]
                ^{:key (+ index 10)}
                [:div
                 [:p.description
                  {:on-click (make-select s index)}
                  (:description option)]
                 (when (= index (:selected s))
                   [:div
                    [:button
                     {:on-click #(grade
                                   (:survey-id s)
                                   (:dimension-id dimension)
                                   (:score option)
                                   (:comment s))}
                     "Continue"]
                    [:textarea.comment
                     {:placeholder "Optional comment"
                      :on-change #(swap! state assoc :comment (-> % .-target .-value))
                      :value (:comment s)}]])])))]])

(defn survey [s]
  (if (< (:dimension s) (count (-> s :survey :template :dimensions)))
    (survey-step s)
    (all-done s)))

(defn loading-display []
  [:p "Loading..."])

(defn error []
  [:div.center
   [:h1 "Oh snap!"]
   [:p.emoji "😭"]
   [:p "Something went very wrong."]])

(defn root []
  (let [s @state]
    (cond
      (:error? s) (error)
      (:loading s) (loading-display)
      :else (case (:screen s)
              :take-survey (survey s)
              (home)))))

(defroute "/survey/:id" {:as params}
  (go
    (let [response (<! (http/get
                         (str endpoint "/api/query/survey/" (:id params))
                         {:with-credentials? false}))]
      (if (= (:status response) 200)
        (swap! state merge
               {:loading false
                :screen :take-survey
                :dimension 0
                :survey-id (:id params)
                :survey (:body response)})
        (swap! state assoc :error? true)))))

(defroute "*" []
  (swap! state assoc :loading false :screen :home))

(defn main []
  (r/render [root] (.getElementById js/document "app")))

(set! (.-onhashchange js/window)
      (fn [_] (secretary/dispatch! (subs js/location.hash 1))))

(when (:loading @state)
 (secretary/dispatch! (subs js/location.hash 1)))

(main)
