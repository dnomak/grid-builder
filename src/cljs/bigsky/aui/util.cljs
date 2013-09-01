(ns bigsky.aui.util
  (:require
   [cljs.core.async :refer [put! chan]]
   [dommy.core :as dom]))

(defn event-chan [event selector msg-name]
  (let [rc (chan)]
    (dom/listen! selector
                 event (fn [e]
                         (put! rc [msg-name e])))
    rc))
