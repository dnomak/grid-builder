(ns shoelace.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.resource :as resources]
            [ring.util.response :as response])
  (:use hiccup.core))

(defn handler [request]
  (cond
   (and (> (count (:uri request)) 8)
        (= "/preview" (subs (:uri request) 0 8)))
      (response/resource-response "preview.html" {:root "public"})
    (= "/" (:uri request))
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
