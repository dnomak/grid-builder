(ns shoelace.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.resource :as resources]
            [ring.util.response :as response])
  (:use hiccup.core)
  (:gen-class))

(defn render-app []
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<!DOCTYPE html>"
     (html
       [:html
         [:head
          [:title "shoelace"]
          [:link {:rel "stylesheet" :href "css/page.css"}]
          [:link {:rel "stylesheet" :href "css/bigsky.aui.css"}]]
         [:body]
           [:script {:src "js/cljs.js"}]]))})

(defn handler [request]
  (when (= "/" (:uri request)))
      (render-app))

(def app
  (-> handler
    (resources/wrap-resource "public")))

(defn run [port]
  (defonce ^:private server
    (jetty/run-jetty app {:port port :join? false}))
  server)

(defn -main
  ([] (run 3030))
  ([port] (run port)))
