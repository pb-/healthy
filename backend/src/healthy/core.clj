(ns healthy.core
  [:require
   [org.httpkit.server :as kit]
   [clojure.string :refer [join]]
   [compojure.core :refer [defroutes GET POST OPTIONS context]]
   [compojure.route :as route]
   [ring.util.request :refer [body-string content-length]]
   [clojure.edn :as edn]
   [ring.middleware.edn :refer [wrap-edn-params]]
   [clojure.spec.alpha :as s]
   [healthy.state :as state]])

(defmulti event-type :type)
(defmethod event-type :survey-created [_]
  (s/keys :req-un [::type ::survey-id ::template-id ::admin-id]))
(defmethod event-type :user-registered [_]
  (s/keys :req-un [::type ::user-id ::user-name]))
(defmethod event-type :dimension-graded [_]
  (s/keys :req-un [::type ::user-id ::survey-id ::dimension-id ::score]))

(s/def ::id-type (s/and string? #(re-matches #"\w{10}" %)))
(s/def ::string+ (s/and string? not-empty))
(s/def ::score (set (range 1 6)))
(s/def ::type keyword?)
(s/def ::template-id ::id-type)
(s/def ::survey-id ::id-type)
(s/def ::dimension-id ::id-type)
(s/def ::admin-id ::id-type)
(s/def ::user-id ::id-type)
(s/def ::user-name ::string+)
(s/def ::comment string?)
(s/def ::event (s/multi-spec event-type :type))

(def storage-file "events.edn")

(def headers
  {"Content-Type" "application/edn" 
   "Access-Control-Allow-Origin" "*"
   "Access-Control-Allow-Headers" "Content-Type"})

(defn load-state []
  (with-open [reader (clojure.java.io/reader storage-file)]
    (reduce state/update-state state/initial (map edn/read-string (line-seq reader)))))

(defn store-event! [event]
  (spit storage-file
        (str (pr-str (assoc event :time (java.util.Date.))) \newline) :append true))

(defn wrap-state [handler]
  (fn [request]
    (handler (assoc request :state (load-state)))))

(defn respond-ok [data]
  {:status 200
   :headers headers
   :body    (pr-str data)})

(defn respond-error [message]
  {:status 400
   :headers headers
   :body    (pr-str {:message message})})

(defn dump [request]
  (respond-ok (:state request)))

(defn handle-survey [request]
  (if-let [survey (state/find-survey-id (:state request) (get-in request [:params :survey-id]))]
    (respond-ok {:template (state/find-template-id (:state request) (:template-id survey))})
    (respond-error "not found")))

(defn handle-check-user [request]
  (let [{:keys [survey-id user-name]} (:params request)
        exists? (state/find-survey-id (:state request) survey-id)]
    (if exists?
      (respond-ok {:graded? (state/has-graded? (:state request) survey-id user-name)})
      (route/not-found "no such survey"))))

(defn handle-event [request]
  (let [event (:edn-params request)]
    (if (s/valid? ::event event)
      (if-let [error (state/error (:state request) event)]
        (respond-error error)
        (do (store-event! event) {:status 201 :headers headers}))
      (respond-error (s/explain-str ::event event)))))

(defroutes routes
  (context "/api" []
           (context "/query" []
                    (GET "/survey/:survey-id" [survey-id] handle-survey)
                    (GET "/survey/:survey-id/user/:user-name" [survey-id user-name] handle-check-user)
                    (GET "/dump" [] dump))
           (context "/command" []
                    (OPTIONS "/" [] {:status 200 :headers headers})
                    (POST "/" [] handle-event)))
  (route/not-found "not here"))

(def app
  (-> routes
      wrap-edn-params
      wrap-state))

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn -main [& args]
  (reset! server (kit/run-server app {:port 8080})))

(comment (-main))
