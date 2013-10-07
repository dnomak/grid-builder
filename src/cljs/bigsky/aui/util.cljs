(ns bigsky.aui.util
  (:require
   [cljs.core.async :refer [put! chan <!]]
   [dommy.core :as dom])
  (:require-macros
   [cljs.core.async.macros :refer [go]]))

(defn spy [x]
  (js/console.log (str x))
  x)

(defn event-chan
  ([selector event msg-name]
     (let [rc (chan)]
       (dom/listen! selector
                    event (fn [e]
                            (put! rc [msg-name e])))
       rc))
  ([selector event]
     (event-chan selector event event)))

(defn go-alphabet
  [& args]
  (let [actions (apply hash-map args)
        in-chan (chan)]
    (go (loop []
          (let [msg (<! in-chan)
                action (actions (first msg))]
            (when action
              (apply action (rest msg)))
            (recur))))
    in-chan))

(defn applies
  [f & args]
  (doall (map #(apply f %) args)))

(defn key->path [o k]
  (apply conj (cons [o] (str/split (name k) #"\."))))

(defn jget [o k]
  (apply aget (key->path o k)))

(defn jset [o k v]
  (apply aset (conj (key->path o k) v)))

(defn insert-after
  [data after val]
  (concat (subvec data 0 after)
          [val]
          (subvec data after)))

(defn watch-change-when
  [data when-fn watch-name handler]
  (add-watch data watch-name
             (fn [k r os ns]
               (when (let [path (when-fn os ns)]
                       (not= (get-in os path)
                             (get-in ns path)))
                 (handler os ns))))
  watch-name)

(defn watch-change-in
  ([data path watch-name handler]
     (watch-change-when data
                        (fn [os ns] path)
                        watch-name
                        (fn [os ns]
                          (handler (get-in os path)
                                   (get-in ns path)))))
  ([data path handler]
     (watch-change-in data path (keyword (apply str "watch-change" (map name path))))))

(defn watch-change
  ([data prop watch-name handler]
     (watch-change-in data [prop] watch-name handler))
  ([data prop handler]
     (watch-change data prop (keyword (str "watch-change" (name prop))) handler)))
