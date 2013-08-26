(ns bigsky.aui.resizable
  (:require
   [dommy.core :as dom]
   [cljs.core.async :refer [>! <! chan sliding-buffer]]
   [bigsky.aui.draggable :refer [draggable]])
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [dommy.macros :refer [node]]))

(def body js/document.body)

(defn resizable [el options]
  (let [in (chan (sliding-buffer 10))
        out (chan (sliding-buffer 10))
        handle-e (node [:.drag-handle.drag-handle-e])
        [handle-e-in handle-e-out] (draggable handle-e)
        size (atom {})
        set-size! #(reset! size {:width (dom/px el "width")
                                 :height (dom/px el "height")})
        calibrate-handles! (fn []
                             (let [cur-x (dom/px el "left")
                                   cur-y (dom/px el "top")
                                   cur-w (dom/px el "width")
                                   cur-h (dom/px el "height")]
                               (dom/set-px! handle-e
                                            :left (- (+ cur-x cur-w) 3)
                                            :top cur-y
                                            :height cur-h)))]
    (dom/append! body handle-e)

    (calibrate-handles!)

    (set-size!)

    (go (loop []
          (let [[msg-name :as msg] (<! in)]
            (when (= msg-name :drag) (calibrate-handles!))
            (recur))))

    (go (loop []
          (let [[msg dx dy] (<! handle-e-out)]
            (when (= msg :drag)
              (dom/set-px! el
                           :width (+ (:width @size) dx)))
            (when (= msg :drag-stop)
              (set-size!)
              (calibrate-handles!))
            (recur))))
    [in out]))
