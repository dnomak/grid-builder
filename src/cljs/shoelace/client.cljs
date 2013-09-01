(ns shoelace.client
  (:require
    [dommy.core :as dom]
    [cljs.core.async :refer [>! <! chan sliding-buffer]]
    [bigsky.aui.util :refer [event-chan]]
    [bigsky.aui.draggable :refer [draggable]])
  (:require-macros
    [cljs.core.async.macros :refer [go]]
    [dommy.macros :refer [node]]))
(defn spy [x]
  (js/console.log (str x))
  x)

(def body js/document.body)

(def id (atom 1))

(defn new-id! [prefix]
  (swap! id inc)
  (str prefix "-" @id))

(def settings (atom {:media-mode :md}))

(def layout (atom []))
(defn get-row
  [row-id]
  (first (filter (fn [r] (= (:id r) row-id))
                 @layout)))

(def sizes [:xs :sm :md :lg])

(def sizes-index {:xs 0 :sm 1 :md 2 :lg 3})

(defn sizes-up-to [max]
  (subvec sizes 0 (inc (sizes-index max))))

(defn cols-for-media [row media]
  (let [sizes (spy (reverse (sizes-up-to media)))]
    (spy (map (fn [col] (first (keep #(% col) sizes)))
              row))))

(def col-width 60)
(def col-height 150)
(def col-margin-width 10)

(defn pt [x y] {:x x :y y})
(defn rect [x y w h]
  {:p1 (pt x y)
   :p2 (pt (+ x w) y)
   :p3 (pt (+ x w) (+ y h))
   :p4 (pt x (+ y h))})

(defn pt-in-rect [p r]
  (and (> (:x p) (:x (:p1 r)))
       (< (:x p) (:x (:p2 r)))
       (> (:y p) (:y (:p1 r)))
       (< (:y p) (:y (:p3 r)))))


(defn get-col-containing [row x y]
  (first (->> (spy (cols-for-media (:cols row)
                                   (@settings :media-mode)))
              (keep-indexed
               (fn [col i]
                 (js/console.log col)
                 (or (pt-in-rect (pt x y)
                                 (rect (+ (* col-width i)
                                          (* col-margin-width i))
                                       0
                                       col-width
                                       col-height))
                     nil))))))

(def snap-threshold 30)

(defn add-col! [new-col-el row-id]
  (let [col-id (new-id! "col")
        col (node [:.col])]
    (dom/append! col (str col-id))
    (dom/insert-before! col new-col-el)
    (dom/listen! col :mousedown
                 (fn [e]
                   (let [start-x (aget e "x")
                         start-w (dom/px col "width")
                         snap! (fn []
                                 (let [w (dom/px col "width")
                                       base (+ col-margin-width col-width)
                                       c (quot w base)
                                       r (mod w base)]
                                   (dom/set-px! col :width
                                                (- (if (> r snap-threshold)
                                                     (* (+ c 1) base)
                                                     (* c  base)) 10))))
                         move-handler (fn [e]
                                        (let [dx (- (aget e "x") start-x)]
                                          (dom/set-px! col :width (+ start-w dx))))
                         stop-handler (fn [e]
                                        (dom/unlisten! js/document :mousemove move-handler)
                                        (snap!))]
                     (dom/listen! js/document :mousemove move-handler)
                     (dom/listen-once! js/document :mouseup stop-handler))))))

(defn add-row! []
  (this-as new-row-el
           (let [row-id (new-id! "row")
                 row (node [:.row])
                 new-col (node [:.new-col])]
             (swap! layout conj {:id row-id :pos (count @layout) :cols []})
             (dom/insert-before! row new-row-el)
             (dom/append! row new-col)
             (dom/listen! new-col
                          :click (fn [e]
                                   (add-col! new-col row-id))))))

(defn draw-workspace []
  (let [workspace (node [:.workspace])
        toolbar (node [:.toolbar])
        medias (node [:.medias])
        media-xs (node [:.media.media-xs [:h4 "xs"]])
        media-tablet (node [:.media.media-tablet [:h4 "sm - @media-tablet"]])
        media-desktop (node [:.media.media-desktop [:h4 "md - @media-desktop"]])
        media-lg-desktop (node [:.media.media-lg-desktop [:h4 "lg - @media-lg-desktop"]])
        container (node [:.container])
        rows (node [:.rows])
        columns (node [:.columns])
        cols (doall (map (fn [i]
                           (let [col (node [:.col])]
                             (dom/append! columns col)
                             col))
                         (range 13)))
        new-row (node [:.row.new-row])]
    (dom/listen! new-row :click add-row!)
    (dom/append! container columns)
    (dom/append! container rows)
    (dom/append! rows new-row)
    (dom/append! medias media-xs)
    (dom/append! medias media-tablet)
    (dom/append! medias media-desktop)
    (dom/append! medias media-lg-desktop)
    (dom/append! workspace medias)
    (dom/append! workspace container)
    (dom/append! body workspace)
    (dom/append! body toolbar)))

(draw-workspace)
