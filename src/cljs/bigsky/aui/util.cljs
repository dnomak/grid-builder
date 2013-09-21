(ns bigsky.aui.util
  (:require
   [cljs.core.async :refer [put! chan]]
   [dommy.core :as dom]))

(defn event-chan
  ([selector event msg-name]
     (let [rc (chan)]
       (dom/listen! selector
                    event (fn [e]
                            (put! rc [msg-name e])))
       rc))
  ([selector event]
     (event-chan selector event event)))

(defn applies
  [f & args]
  (doall (map #(apply f %) args)))
