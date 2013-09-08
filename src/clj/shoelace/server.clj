(ns shoelace.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.resource :as resources]
            [ring.util.response :as response])
  (:use hiccup.core))

(defn handler [request]
  (when (= "/" (:uri request))
    (response/resource-response "index.html" {:root "public"})))

(def app
  (-> handler
    (resources/wrap-resource "public")))

(defn run [port]
  (defonce ^:private server
    (jetty/run-jetty app {:port (Integer. port) :join? false}))
  server)

(defn -main
  ([] (run 3030))
  ([port] (run port)))
