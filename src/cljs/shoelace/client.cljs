(ns shoelace.client
  (:require
   [dommy.core :as dom]
   [dommy.utils :refer [dissoc-in]]
   [cljs.core.async :refer [>! <! chan sliding-buffer]]
   [bigsky.aui.util :refer [event-chan]]
   [bigsky.aui.draggable :refer [draggable]])
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [dommy.macros :refer [node sel1]]))

(defn spy [x]
  (js/console.log (str x))
  x)

(def col-offset-pos 0)

(def col-width-pos 1)

(def col-width 60)

(def col-height 150)

(def col-margin-width 10)

(def snap-threshold 20)

(def grid-cols 12)

(def body js/document.body)

(def id (atom 1))

(defn new-id! [prefix]
  (swap! id inc)
  (keyword (str prefix "-" @id)))

(def settings (atom {:media-mode :md
                     :active-col :none}))

(def layout (atom []))

(defn get-by-key
  [col attr val]
  (first (filter (fn [i] (= (attr i) val)) col)))

(defn get-by-id
  [col id]
  (get-by-key col :id id))

(defn get-row
  [row-id]
  (get-by-id @layout row-id))

(defn get-col
  [row-id col-id]
  (get-by-id ((get-row row-id) :cols) col-id))

(def sizes [:xs :sm :md :lg])

(def sizes-index {:xs 0 :sm 1 :md 2 :lg 3})

(defn sizes-up-to
  [max]
  (reverse (subvec sizes 0 (inc (sizes-index max)))))

(def sizes-up-to-memo (memoize sizes-up-to))

(defn col-for-media
  [col media]
  (first (keep #(% col) (sizes-up-to-memo media))))

(defn cols-for-media
  [row media]
  (map #(col-for-media % media) (:cols row)))

(defn total-cols-used
  [row media]
  (apply + (flatten (cols-for-media row media))))

(defn get-el [id]
  (sel1 (keyword (str "#" (name id)))))

(defn add-col!
  [e cols-el new-col-el row-id]
  (let [col-id (new-id! 'col)
        col-el (node [:.col {:id (name col-id)}])
        offset-el (node [:.offset])
        remove-el (node [:.remove "remove"])
        width-el (node [:.width])
        els {:width width-el
             :offset offset-el}
        type-pos {:offset 0
                  :width 1}
        offset-handle-el (node [:.offset-handle])
        row (get-row row-id)
        total-cols (fn [] (total-cols-used (get-row row-id) (@settings :media-mode)))
        check-to-hide-new-col (fn [] (if (= grid-cols (total-cols))
                                      (dom/add-class! new-col-el :hidden)
                                      (dom/remove-class! new-col-el :hidden)))
        active-col (fn [e]
                     (.stopPropagation e)
                     (when (not (= (:active-col @settings) :none))
                       (dom/remove-class! (get-el (:active-col @settings)) :active))
                     (swap! settings assoc :active-col col-id)
                     (dom/add-class! col-el :active))
        handle-remove (fn [e]
                        (.stopPropagation e)
                        (let [row (get-row row-id)]
                          (swap! settings assoc :active-col :none)
                          (swap! layout assoc-in
                                 [(:pos row) :cols]
                                 (into [] (map-indexed (fn [i c] (assoc c :pos i))
                                                       (filter (fn [c] (not (= (:id c) col-id)))
                                                               (get-in @layout [(:pos row) :cols])))))
                          (dom/remove! col-el)
                          (check-to-hide-new-col)))
        handle-drag (fn [type e]
          (.stopPropagation e)
          (active-col e)
          (let [start-x (aget e "x")
                start-w (dom/px (els type) "width")
                media (@settings :media-mode)
                col-unit (+ col-margin-width col-width)
                row (get-row row-id)
                col (get-col row-id col-id)
                cur-cols-used (col-for-media col media)
                max-cols (- grid-cols (total-cols-used row media))
                max-width (- (* (+ (cur-cols-used (type-pos type)) max-cols) col-unit) 10)
                snap! (fn []
                        (let [w (+ (if (= type :offset) 10 0)
                                   (dom/px (els type) "width"))
                              c (quot w col-unit)
                              r (mod w col-unit)
                              new-width (if (= type :offset)
                                          (max c 0)
                                          ((if (> r snap-threshold) + max) c 1))]
                          (swap! layout assoc-in
                                 [(:pos row) :cols (:pos col) media (type-pos type)]
                                 new-width)
                          (dom/set-px! (els type) :width (- (* new-width col-unit) (if (= type :width) 10 0)))))
                valid-step (fn [width]
                             (let [c (quot width col-unit)]
                               (or
                                (< c (cur-cols-used (type-pos type)))
                                (and (= max-cols 0)
                                     (< c (cur-cols-used (type-pos type))))
                                (and (or (= type :offset) (> c 0))
                                     (< c (+ max-cols (cur-cols-used (type-pos type))))))))
                move-handler (fn [e]
                               (let [dx (- (aget e "x") start-x)
                                     sdx (+ start-w dx)
                                     nw (if (> sdx  max-width) max-width sdx)]
                                 (when (valid-step nw)
                                   (dom/set-px! (els type) :width nw))))
                stop-handler (fn [e]
                               (dom/unlisten! js/document :mousemove move-handler)
                               (snap!)
                               (check-to-hide-new-col))]
            (dom/add-class! new-col-el "hidden")
            (dom/listen! js/document :mousemove move-handler)
            (dom/listen-once! js/document :mouseup stop-handler)))]

    (swap! layout update-in [(:pos row) :cols] conj {:id col-id
                                                     :pos (count (:cols row))
                                                     (@settings :media-mode) [0 1]})
    (dom/append! width-el offset-handle-el)
    (dom/append! col-el offset-el)
    (dom/append! col-el width-el)
    (dom/append! col-el remove-el)
    (dom/append! cols-el col-el)
    (check-to-hide-new-col)
    (dom/remove-class! new-col-el "no-cols")
    (dom/listen! remove-el :mousedown #(handle-remove %))
    (dom/listen! offset-handle-el :mousedown #(handle-drag :offset %))
    (dom/listen! offset-el :mousedown #(handle-drag :offset %))
    (dom/listen! width-el :mousedown #(handle-drag :width %))

    (handle-drag :width e)))

(defn add-row! []
  (this-as new-row-el
           (let [row-id (new-id! "row")
                 row (node [:.row])
                 cols (node [:.cols])
                 new-col (node [:.new-col.no-cols])]
             (swap! layout conj {:id row-id :pos (count @layout) :cols []})
             (dom/insert-before! row new-row-el)
             (dom/append! row cols)
             (dom/append! row new-col)
             (dom/listen! new-col
                          :mousedown (fn [e]
                                       (add-col! e cols new-col row-id))))))

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
                         (range grid-cols)))
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

(dom/listen! js/document :mousedown (fn []
                           (when (not (= (:active-col @settings) :none))
                             (dom/remove-class! (get-el (:active-col @settings)) :active))
                           (swap! settings assoc :active-col :none)))
(draw-workspace)
