(ns bigsky.aui.draggable
  (:require [dommy.core :as dom]
            [cljs.core.async :refer [>! <! chan sliding-buffer]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn draggable
  ([el]
     (draggable el nil))

  ([el sel]
     (let [in (chan (sliding-buffer 10))
           out (chan (sliding-buffer 10))
           pos (atom {})
           set-pos! #(reset! pos {:x (or (dom/px el "left") 0)
                                  :y (or (dom/px el "top") 0)})
           move-el! (fn [dx dy]
                      (let [nx (+ dx (:x @pos))
                            ny (+ dy (:y @pos))]
                        (dom/set-px! el :left nx :top ny)))
           start-drag (fn [e]
                        (let [start-e-x (aget e "x")
                              start-e-y (aget e "y")
                              drag-move (fn [e]
                                          (let [dx (- (aget e "x") start-e-x)
                                                dy (- (aget e "y") start-e-y)]
                                            (go (>! out [:drag dx dy])
                                                (move-el! dx dy))))
                              drag-stop (fn [e]
                                          (go (set-pos!)
                                              (>! out [:drag-stop 0 0])
                                              (dom/unlisten! js/document :mousemove drag-move)))]
                          (go (>! out [:drag-start 0 0])
                              (dom/listen! js/document :mousemove drag-move)
                              (dom/listen-once! js/document :mouseup drag-stop))))]

       (set-pos!)
       (go (loop []
             (let [[msg dx dy] (<! in)]
               (when (= msg :drag) (move-el! dx dy))
               (when (= msg :drag-stop) (set-pos!))
               (recur))))

       (dom/listen! (if (nil? sel) el [el sel]) :mousedown start-drag)
       (dom/add-class! el :aui-draggable)
       [in out])))
