(ns shoelace.client
  (:require
    [dommy.core :as dom]
    [cljs.core.async :refer [>! <! chan sliding-buffer]])
  (:require-macros
    [cljs.core.async.macros :refer [go]]
    [dommy.macros :refer [node]]))

(def body js/document.body)

(dom/append! body (node [:h1 "shoelace"]))
