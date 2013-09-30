(ns ednio.core
  (:require
   [ajax.core :as ajax]
   [cljs.core.async :refer [>! <! chan]])
  (:require-macros
   [cljs.core.async.macros :refer [go]]))

(def url "http://edn.io/")

(defn get
  [k]
  (let [out-chan (chan)]
    (ajax/GET (str url k)
              {:handler #(go (>! out-chan %))})
    out-chan))

(defn post
  [k v]
  (let [out-chan (chan)]
    (ajax/POST (str url k)
               {:params v
                :format :edn
                :handler #(go (>! out-chan %))})
    out-chan))
