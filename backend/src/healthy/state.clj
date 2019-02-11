(ns healthy.state
  [:require
   [clojure.set :as s]])

(def initial
  {:surveys []
   :users []
   :templates [{:template-id "8H1GxragdS"
                :title "Agile health check"
                :dimensions [{:dimension-id "b96LyQtmbz"
                              :name "Agility"
                              :options [{:option-id "UVGJPycs5x"
                                         :score 1
                                         :description "Team rarely completes estimated work on time. Team has no concept of small, iterative efforts, but practices big upfront design. Team jumps straight into designing solutions without a hypothesis. Team is unable to adapt and re-focus quickly if priorities change."}
                                        {:option-id "6FBrhRL5yW"
                                         :score 2
                                         :description "Team sometimes completes estimated work on time. Team has some understanding of breaking down big tasks into smaller, testable pieces. Team jumps to solutions frequently but starts asking about the problem. Team is somewhat able to adapt and re-focus quickly if priorities change."}
                                        {:option-id "0Vc4onvfnV"
                                         :score 3
                                         :description "Team usually completes the estimated work on time. Team has understanding of breaking down larger problems, introducing concepts like prototyping, A/B testing, and MVP's. Team starts many efforts by writing down the problem it tries to solve, starts to be aware of the business impact. Team is able to adapt and re-focus quickly if priorities change, but with negative impact to the current velocity."}
                                        {:option-id "Q2rKKs10lK"
                                         :score 4
                                         :description "Team mostly completes estimated work on time. Team has actively used concepts like prototyping, A/B testing, and MVP's. Team starts majority of the efforts by framing the problem and starts thinking about the potential businees impact. Team is able to adapt and re-focus quickly if priorities change, with some impact to the current velocity."}
                                        {:option-id "P01EnjiZwG"
                                         :score 5
                                         :description "Team has a self designed workflow visualisation and information is continuously radiated to the office. Team almost always completes estimated work on time. Team has understanding and deep experience of prototpying, A/B tests, MVPs, and adopt it in all scenarios. Team starts every effort by framing the problem and impact in an hypothesis, leveraging past learnings, iterating and documenting it for everyone to see. Team is able to adapt and re-focus quickly if priorities change."}]}]}]})

(defn normalize [user-name]
  (->> user-name
       .trim
       .toLowerCase))

; will need some abstraction here
(defn find-user-id [state id]
  (first (filter #(= id (:user-id %)) (:users state))))

(defn find-user-names [state user-name]
  (filter #(= (normalize user-name) (normalize (:user-name %))) (:users state)))

(defn find-template-id [state id]
  (first (filter #(= id (:template-id %)) (:templates state))))

(defn find-admin-id [state id]
  (first (filter #(= id (:admin-id %)) (:surveys state))))

(defn find-survey-id [state id]
  (first (filter #(= id (:survey-id %)) (:surveys state))))

(defn find-survey-admin-id [state id]
  (first (filter #(= id (:admin-id %)) (:surveys state))))

(defn survey-user-ids [survey]
  (->> survey :grades vals (map keys) flatten))

(defn has-graded? [state survey-id user-name]
  (let [survey (find-survey-id state survey-id)
        user-ids (set (map :user-id (find-user-names state user-name)))]
    (->> survey
         survey-user-ids
         set
         (s/intersection user-ids)
         empty?
         not)))

(defn status [state survey]
  (let [dims (->> (:template-id survey) (find-template-id state) :dimensions count)]
    (->> survey
         user-ids
         (reduce (fn [counts, id] (update counts id #(inc (or % 0)))) {})
         (map (fn [[id n]]
                {:user-name (:user-name (find-user-id state id))
                 :finished? (= n dims)})))))

(defmulti ^:private update-unsafe (fn [_ event] (:type event)))

(defmulti error (fn [_ event] (:type event)))

(defmethod update-unsafe :default [state _]
  state)

(defmethod error :default [_ _]
  nil)

(defmethod error :survey-created [state event]
  (cond (or (find-survey-id state (:survey-id event))
            (find-survey-admin-id state (:admin-id event))) "survey already exists"
        (not (find-template-id state (:template-id event))) "no such template"
        :else nil))

(defmethod update-unsafe :survey-created [state event]
  (update state :surveys
          #(conj % (assoc (select-keys event [:survey-id :template-id :admin-id])
                          :created-at (:time event)
                          :grades {}
                          :ended? false))))

(defmethod error :survey-ended [state event]
  (if (find-admin-id state (:admin-id event))
    nil
    "no such survey"))

(defmethod update-unsafe :survey-ended [state event]
  (let [survey-id (->> event :admin-id (find-admin-id state) :survey-id)]
    (update state :surveys
            (partial
              mapv (fn [survey]
                     (if (= (:survey-id survey) survey-id)
                       (assoc survey :ended? true)
                       survey))))))

(defmethod error :user-registered [state event]
  (if (find-user-id state (:user-id event))
    "user already exists"
    nil))

(defmethod update-unsafe :user-registered [state event]
  (update state :users
          #(conj % (assoc (select-keys event [:user-id :user-name])
                          :registered-at (:time event)))))

(defmethod error :dimension-graded [state event]
  (if (not (find-user-id state (:user-id event)))
    "no such user"
    (if-let [survey (find-survey-id state (:survey-id event))]
      (if (some #(= (:dimension-id event) (:dimension-id %))
                (:dimensions (find-template-id state (:template-id survey))))
        nil
        "no such dimension")
      "no such survey")))

(defmethod update-unsafe :dimension-graded [state event]
  (assoc state :surveys
         (mapv (fn [survey]
                 (if (= (:survey-id survey) (:survey-id event))
                   (assoc-in survey
                             [:grades (:dimension-id event) (:user-id event)]
                             (select-keys event [:score :comment]))
                   survey))
               (:surveys state))))

(defn update-state [state event]
  (if (error state event)
    state
    (update-unsafe state event)))
