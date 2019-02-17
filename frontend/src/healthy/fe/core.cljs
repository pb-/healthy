(ns ^:figwheel-hooks healthy.fe.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [secretary.core :as secretary :refer-macros [defroute]]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [goog.functions :refer [debounce]]))

(def endpoint (str js/document.location.protocol "//" js/document.location.hostname ":8080"))
(def address (str js/document.location.protocol "//" js/document.location.host "/"))
(defonce state (r/atom {:loading true}))
(secretary/set-config! :prefix "#")

(defn make-id []
  (->> #(rand-nth "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz")
       (repeatedly 10)
       (apply str)))

(defn home []
  [:p "Welcome to agile health check!"])

(defn make-select [s i]
  (fn []
    (swap! state assoc :selected
           (if (= (:selected s) i)
             nil
             i))))

(defn grade [survey-id user-id dimension-id score grade-comment]
  (go (let [response (<! (http/post
                           (str endpoint "/api/command")
                           {:with-credentials? false
                            :edn-params {:type :dimension-graded
                                         :user-id user-id
                                         :survey-id survey-id
                                         :dimension-id dimension-id
                                         :score score
                                         :comment grade-comment}}))]
        (if (= (:status response) 201)
          (swap! state #(-> % (update-in [:dimension] inc) (dissoc :comment :selected)))
          (swap! state assoc :error? true)))))

(defn survey-ended [s]
  [:div.center
   [:h1 "Survey has ended"]
   [:p.emoji "⏳"]
   [:p "Stay tuned for results!"]])

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
   [:div.panel
    (concat (for [index (range 5)]
              ^{:key index}
              [:p
               {:class (str "score clickable" (when (or (not (:selected s)) (= (:selected s) index)) (str " s" index)))
                :on-click (make-select s index)} (inc index)])
            (for [index (range 5)]
              (let [dimension ((get-in s [:survey :template :dimensions]) (:dimension s))
                    option ((:options dimension) index)]
                ^{:key (+ index 10)}
                [:div
                 [:p.description.clickable
                  {:on-click (make-select s index)}
                  (:description option)]
                 (when (= index (:selected s))
                   [:div
                    [:button.fullwidth
                     {:on-click #(grade
                                   (:survey-id s)
                                   (:user-id s)
                                   (:dimension-id dimension)
                                   (:score option)
                                   (:comment s))}
                     "Continue"]
                    [:textarea.comment
                     {:placeholder "Optional comment"
                      :on-change #(swap! state assoc :comment (-> % .-target .-value))
                      :value (:comment s)}]])])))]])

(defn register [s]
  (go (let [user-id (make-id)
            response (<! (http/post (str endpoint "/api/command")
                                    {:with-credentials? false
                                     :edn-params {:type :user-registered
                                                  :user-id user-id
                                                  :user-name (:user-name s)}}))]

        (swap! state assoc :user-id user-id))))

(defn check-name [s user-name submit?]
  (go (let [response (<! (http/get
                           (str endpoint "/api/query/survey/" (:survey-id s) "/user/"
                                (js/encodeURIComponent user-name))
                           {:with-credentials? false}))
            available? (-> response :body :graded? not)]
        (do
          (swap! state assoc :name-available? available?)
          (when (and submit? available?) (register s))))))

(def check-name' (debounce check-name 333))

(defn name-changed [s user-name]
  (do (swap! state assoc :user-name user-name :name-available? true)
      (check-name' s user-name false)))

(defn can-register? [s]
  (and (:name-available? s)
       (not-empty (:user-name s))))

(defn register-form [s]
  (let [submit #(check-name s (:user-name s) true)]
    [:div.register
     [:h1 (get-in s [:survey :template :title])]
     [:input {:type "text"
              :value (:user-name s)
              :placeholder "Your name"
              :auto-focus true
              :on-change #(->> % .-target .-value (name-changed s))
              :on-key-press #(when (and (can-register? s)
                                        (= 13 (.-charCode %))) (submit))}] " "
     [:button {:disabled (not (can-register? s))
               :on-click submit}
      "Let's go"]
     (when (not (:name-available? s true))
       [:p.smaller "That name is already in use."])]))

(defn survey [s]
  (if (< (:dimension s) (count (-> s :survey :template :dimensions)))
    (if (:user-id s)
      (survey-step s)
      (register-form s))
    (all-done s)))

(declare admin-path)

(defn close-survey [s]
  (go
    (let [response (<! (http/post (str endpoint "/api/command")
                                    {:with-credentials? false
                                     :edn-params {:type :survey-ended
                                                  :admin-id (:admin-id s)}}))]
      (if (= (:status response) 201)
        (secretary/dispatch! (admin-path {:id (:admin-id s)}))
        (swap! state :error? true)))))

(declare survey-path)

(defn admin-progress [s]
  (let [{:keys [status survey-id]} (:admin s)]
    [:div.progress
     [:h1 "Survey in progress"]
     [:p.label "Public survey link for participants:"]
     [:p.link
      (let [url (str address (survey-path {:id survey-id}))]
        [:a {:href url} url])]
     [:p.label "Secret admin link for you (don't lose it!):"]
     [:p.link.adminlink js/document.location.href]
     [:p "This survey is in progress. Once you close it, no further responses are accepted."]
     [:button.fullwidth {:on-click #(close-survey s)} "Close survey"]
     (when (not-empty status)
       [:div
        [:h2 "Respondents"]
        [:table.respondents
         [:tbody
          (for [respondent (sort-by #(.toLowerCase (:user-name %)) status)]
            ^{:key (:user-name respondent)}
            [:tr
             [:td (:user-name respondent)]
             [:td (if (:finished? respondent) "finished 🏁" "started ⌛")]])]]])]))

(defn medians [scores]
  (let [sorted (->> scores (map (fn [[k v]] (repeat (count v) k))) flatten sort)
        len (count sorted)
        middle (quot len 2)
        middle-1 (dec middle)]
    (cond
      (empty? sorted) #{}
      (odd? len) #{(nth sorted middle)}
      :else #{(nth sorted middle) (nth sorted middle-1)})))

(defn admin-results [s]
  [:div
   (for [dimension (-> s :admin :template :dimensions)]
     (let [columns (->> dimension :options count inc (range 1) vec)
           scores (get (-> s :admin :grades) (:dimension-id dimension))
           meds (medians scores)]
       [:div
        [:h2 (:name dimension)]
        [:div.panel
         (concat
           (for [score columns]
             [:p {:class (str "score" (when (meds score) (str " s" score)))} score])
           (for [score columns]
             [:div
              (for [user (get scores score)]
                [:div.user (:user-name user) " "])])
           (for [option (:options dimension)]
             [:p.description (:description option)]))]]))])

(defn admin [s]
  (if (get-in s [:admin :ended?])
    (admin-results s)
    (admin-progress s)))

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
              :ended (survey-ended s)
              :take-survey (survey s)
              :admin (admin s)
              (home)))))

(defroute admin-path "/admin/:id" {:as params}
  (go
    (let [response (<! (http/get
                         (str endpoint "/api/query/admin/" (:id params))
                         {:with-credentials? false}))]
      (if (= (:status response) 200)
        (swap! state merge
               {:loading false
                :error? false
                :screen :admin
                :admin-id (:id params)
                :admin (:body response)})
        (swap! state assoc :error? true)))))

(defroute survey-path "/survey/:id" {:as params}
  (go
    (let [response (<! (http/get
                         (str endpoint "/api/query/survey/" (:id params))
                         {:with-credentials? false}))]
      (if (= (:status response) 200)
        (swap! state merge
               {:loading false
                :error? false
                :screen (if (-> response :body :ended?) :ended :take-survey)
                :dimension 0
                :survey-id (:id params)
                :survey (:body response)})
        (swap! state assoc :error? true)))))

(defroute "*" []
  (swap! state assoc :loading false :error? false :screen :home))

(defn main []
  (r/render [root] (.getElementById js/document "app")))

(set! (.-onhashchange js/window)
      (fn [_] (secretary/dispatch! (subs js/location.hash 1))))

(when (:loading @state)
 (secretary/dispatch! (subs js/location.hash 1)))

(main)
