(ns gist.core
  (:require
   [ajax.core :as ajax]
   [clojure.string :as string]
   [cljs.reader :refer [read-string]]))

(def url "https://api.github.com/")

(defn- crud
  [path description content handler]
  (.post js/$
         (str url path)
         (.stringify js/JSON (clj->js {"description" description
                                       "public" true
                                       "files" {"grid.edn" {"content" content}}}))
         handler))

(defn create
  [description content handler]
  (crud "gists" description content handler))

(defn update
  [id description content handler]
  (crud (str "gists/" id) description content handler))

(defn fetch
  [id handler]
  (.get js/$ (str url "gists/" id) handler))

(defn encode-id
  [id]
  (string/join
   ""
   (let [r (int (rand 9))]
     (concat [(char (+ 70 r))]
             (map #(char (+ (+ 70 r)
                            (read-string (str %))))
                  (into [] (str id)))))))

(defn decode-id
  [sid]
  (let [[r & cs] (into [] sid)
        ri (- (.charCodeAt r 0) 70)]
    (read-string (string/join
                  ""
                  (map #(str (- (.charCodeAt % 0) (+ 70 ri))) cs)))))
