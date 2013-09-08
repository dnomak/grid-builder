(ns shoelace.client
  (:require
   [clojure.string :refer [join]]
   [hiccups.runtime :as hrt]
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

(def col-height 150)

(def col-width 60)

(def col-margin-width 15)

(def snap-threshold 20)

(def grid-cols 12)

(def body js/document.body)

(def id (atom 1))

(defn new-id! [prefix]
  (swap! id inc)
  (keyword (str prefix "-" @id)))

(def settings (atom {:media-mode :sm
                     :active-col :none
                     :include-container true
                     :output-mode :html}))

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

(def size-labels
  {:xs "xs"
   :sm "sm - @media-tablet"
   :md "md - @media-desktop"
   :lg "lg - @media-lg-desktop"})


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

(defn calc-col-unit []
  (+ col-margin-width col-width))

(defn draw-rows []
  (let [col-unit (calc-col-unit)
        media (:media-mode @settings)]
    (doseq [row @layout]
      (spy col-unit)
      (doseq [col (:cols row)]
        (spy col)
        (spy (col-for-media col media))
        (let [col-el (get-el (:id col))
              [offset width] (spy (col-for-media col media))]
          (dom/set-px! (sel1 [col-el :.offset]) :width (* offset col-unit))
          (dom/set-px! (sel1 [col-el :.width]) :width (* width col-unit)))))))

(defn  get-class-el
  [col-id size type]
  (sel1 (str "#" (name col-id) " ." (name size) "-" (name type))))

(defn add-col!
  [e cols-el new-col-el row-id]
  (let [col-id (new-id! 'col)
        col-el (node [:.col {:id (name col-id)}])
        offset-el (node [:.offset])
        remove-el (node [:.remove "x"])
        width-el (node [:.width])
        classes-el (node [:.classes
                          [:.xs-width] [:.xs-offset]
                          [:.sm-width] [:.sm-offset]
                          [:.md-width] [:.md-offset]
                          [:.lg-width] [:.lg-offset]])
        name-el (node [:input.col-name {:placeholder "col"}])
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
                col-unit (calc-col-unit)
                row (get-row row-id)
                col (get-col row-id col-id)
                cur-cols-used (col-for-media col media)
                max-cols (- grid-cols (total-cols-used row media))
                max-width (- (* (+ (cur-cols-used (type-pos type)) max-cols) col-unit) col-margin-width)
                snap! (fn []
                        (let [w (+ (if (= type :offset) col-margin-width 0)
                                   (dom/px (els type) "width"))
                              c (quot w col-unit)
                              r (mod w col-unit)
                              new-width (if (= type :offset)
                                          (max c 0)
                                          ((if (> r snap-threshold) + max) c 1))]
                          (swap! layout assoc-in
                                 [(:pos row) :cols (:pos col) media (type-pos type)]
                                 new-width)
                          (dom/set-text! (get-class-el col-id media type)
                                         (str (name media) "-"
                                              (when (= type :offset) "offset-")
                                              new-width))
                          (dom/set-px! (els type)
                                       :width
                                       (- (* new-width col-unit)
                                          (if (= type :width) col-margin-width 0)))))
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
    (dom/append! width-el name-el)
    (dom/append! width-el classes-el)
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
                 row (node [:.sl-row])
                 cols (node [:.cols])
                 name-el (node [:input.row-name {:placeholder "Name Row"}])
                 tools-el (node [:.tools [:span "d"] [:span "r"] [:span "x"]])
                 new-col (node [:.new-col.no-cols])]
             (swap! layout conj {:id row-id :pos (count @layout) :cols []})
             (dom/append! row name-el)
             (dom/append! row tools-el)
             (dom/insert-before! row new-row-el)
             (dom/append! row cols)
             (dom/append! row new-col)
             (dom/listen! new-col
                          :mousedown (fn [e]
                                       (add-col! e cols new-col row-id))))))

(defn layout->html
  [rows]
  (letfn [(size-classes [c]
            (apply str
             (flatten
              (map (fn [s]
                     (if (s c)
                       (let [[offset width] (s c)]
                         [(when (> offset 0)
                            (str ".col-" (name s) "-offset-" offset))
                          (str ".col-" (name s) "-" width)])))
                   sizes))))]
    (map
     (fn [r]
       (conj [:div.row]
             (map (fn [c]
                    [(keyword (str "div" (size-classes c)))])
                  (:cols r))))
     rows)))

(defn draw-workspace []
  (let [workspace (sel1 :.workspace)
        output (sel1 :pre.output)
        container (node [:.container])
        rows (node [:.rows])
        columns (node [:.columns])
        new-row (node [:.sl-row.new-row])
        media-mode (:media-mode @settings)]
    (dom/add-class! container media-mode)

    (doseq [i (range grid-cols)]
      (let [col (node [:.col])]
        (dom/append! columns col)))

    (doseq [[size label] size-labels]
      (let [media (sel1 (str ".preview." (name size)))]
        (when (= size media-mode) (dom/add-class! media :active))
        (dom/listen! media :mouseup #(swap! settings assoc :media-mode size))))

    (add-watch settings :update-media
               (fn [k r os ns]
                 (when (not (= (:media-mode os) (:media-mode ns)))
                   (dom/remove-class! container (:media-mode os))
                   (dom/remove-class! (sel1 :.preview.active) :active)
                   (dom/add-class! (sel1 (str  ".preview." (name (:media-mode ns)))) :active)
                   (dom/add-class! container (:media-mode ns)))))

    (add-watch layout :update-output
               (fn [k r os ns]
                 (dom/remove-class! output :prettyprinted)
                 (dom/set-text! output
                  (condp = (:output-mode @settings)
                    :html (js/html_beautify
                           (hrt/render-html
                            (conj [:div.container] (layout->html ns))))

                    :edn (str (mapv (fn [r] (mapv (fn [c] (dissoc c :id :pos)) (:cols r))) ns))))
                 (js/PR.prettyPrint)))


    (dom/listen! new-row :click add-row!)
    (dom/append! container columns)
    (dom/append! container rows)
    (dom/append! rows new-row)
    (dom/append! workspace container)))

(dom/listen! js/document
             :mousedown
             (fn []
               (when (not (= (:active-col @settings) :none))
                 (dom/remove-class! (get-el (:active-col @settings)) :active))
               (swap! settings assoc :active-col :none)))

(draw-workspace)
