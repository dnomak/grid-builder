(ns gist.core
  (:require
   [ajax.core :as ajax]
   [clojure.string :as string]
   [cljs.reader :refer [read-string]]))

(def url "https://api.github.com/")

(defn- crud
  [verb path description files handler]
  (.ajax js/$
         (clj->js
          {"url" (str url path)
           "type" verb
           "data" (.stringify js/JSON
                              (clj->js {"description" description
                                        "public" true
                                        "files" files }))
           "success" handler})))

(defn create
  [description content handler]
  (crud "POST" "gists" description {"grid.edn" {"content" content}} handler))

(defn update
  [id description content handler]
  (crud "POST" "gists" description {"grid.edn" {"content" content}
                                    "prior.edn" {"content" (str [id])}} handler))

(defn fetch
  [id handler]
  (.get js/$ (str url "gists/" id) handler))

(def encode-id identity)

(def decode-id identity)
