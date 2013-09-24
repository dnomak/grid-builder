(ns shoelace.client
  (:require
   [clojure.string :refer [join split]]
   [hiccups.runtime :as hrt]
   [dommy.core :as dom]
   [dommy.utils :refer [dissoc-in]]
   [cljs.core.async :refer [>! <! put! chan sliding-buffer]]
   [bigsky.aui.util :refer [event-chan applies]]
   [bigsky.aui.draggable :refer [draggable]])
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [dommy.macros :refer [node sel sel1]]))

(defn sels
  [names selectors]
  (zipmap names (for [s selectors] (sel1 s))))

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

(defn insert-after
  [data after val]
  (concat (subvec data 0 after)
          [val]
          (subvec data after)))

(defn watch-change
  ([data prop watch-name handler]
     (add-watch data watch-name
                (fn [k r os ns]
                  (when (not (= (prop os) (prop ns)))
                    (handler (prop os) (prop ns)))))
    watch-name)
  ([data prop handler]
     (watch-change data prop (keyword (str "watch-change" (name prop))) handler)))

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
                     :include-container true
                     :active-row :none
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
  (get-by-id (:cols (get-row row-id)) col-id))

(def sizes [:xs :sm :md :lg])

(def sizes-index {:xs 0 :sm 1 :md 2 :lg 3})

(defn sizes-up-to
  [max]
  (reverse (subvec sizes 0 (inc (sizes-index max)))))

(def sizes-up-to-memo (memoize sizes-up-to))

(defn col-for-media
  [col media]
  (let [found (first (keep #(% col) (sizes-up-to-memo media)))]
    (if found
      found
      [0 grid-cols])))

(defn cols-for-media
  [row media]
  (map #(col-for-media % media) (:cols row)))

(defn total-cols-used
  [row media]
  (apply + (flatten (cols-for-media row media))))

(defn id->sel [id]
  (str "#" (name id)))

(defn get-el [id]
  (sel1 (id->sel id)))

(defn calc-col-unit []
  (+ col-margin-width col-width))

(defn get-class-el
  [col-id size type]
  (sel1 (str "#" (name col-id) " ." (name size) "-" (name type))))

(defn row-wraps-for-media?
  [row media]
  (or (:wrap row)
      (> (total-cols-used row media) grid-cols)))

(defn add-col!
  [e cols-el new-col-el row-id]
  (let [col-id (new-id! 'col)
        col-el (node [:.col {:id (name col-id)}])
        offset-el (node [:.offset])
        remove-el (node [:.remove [:i.icon-remove]])
        width-el (node [:.width])
        nested-el (node [:.nested [:i.icon-th]])
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
        handle-remove (fn [e]
                        (.stopPropagation e)
                        (let [row (get-row row-id)
                              path [(:pos row) :cols]]
                          (swap! layout assoc-in path
                                 (into []
                                       (map-indexed (fn [i c] (assoc c :pos i))
                                                    (filter (fn [c] (not (= (:id c) col-id)))
                                                            (get-in @layout path)))))
                          (dom/remove! col-el)
                          (check-to-hide-new-col)
                          (when (zero? (count (get-in @layout path)))
                            (dom/add-class! new-col-el :no-cols))))
        draw-class-type (fn [media type size]
                          (dom/set-text! (get-class-el col-id media type)
                                         (if (and (= type :offset) (zero? size))
                                           ""
                                           (str (name media) "-"
                                                (when (= type :offset) "offset-")
                                                size))))
        draw-classes (fn []
                       (let [col (get-col row-id col-id)]
                         (doseq [media sizes]
                           (when (media col)
                             (let [[offset width] (media col)]
                               (when offset (draw-class-type media :offset offset))
                               (when width (draw-class-type media :width width)))))))

        handle-drag (fn [type e]
          (.stopPropagation e)
          (let [start-x (aget e "x")
                start-w (dom/px (els type) "width")
                media (@settings :media-mode)
                col-unit (calc-col-unit)
                row (get-row row-id)
                col (get-col row-id col-id)
                cur-cols-used (col-for-media col media)
                max-cols (- grid-cols (total-cols-used row media))
                max-width (- (* (if (row-wraps-for-media? row media)
                                  grid-cols
                                  (+ (cur-cols-used (type-pos type)) max-cols))
                                col-unit)
                             col-margin-width)
                snap! (fn []
                        (let [w (+ (if (= type :offset) col-margin-width 0)
                                   (dom/px (els type) "width"))
                              c (quot w col-unit)
                              r (mod w col-unit)
                              new-width (if (= type :offset)
                                          (max c 0)
                                          ((if (> r snap-threshold) + max) c 1))
                              path [(:pos row) :cols (:pos col) media]
                              new-dims (assoc-in (or (get-in @layout path)
                                                     [0 1])
                                                 [(type-pos type)]
                                                 new-width)]
                          (swap! layout assoc-in path new-dims)
                          (draw-class-type media type new-width)
                          (dom/add-class! (els type) :easing)
                          (dom/set-px! (els type)
                                       :width
                                       (- (* new-width col-unit)
                                          (if (= type :width) col-margin-width 0)))))
                valid-step (fn [width]
                             (let [c (quot width col-unit)]
                               (or
                                ;;if they are wrapping we stop constraining
                                (and (row-wraps-for-media? row media) (< (+ c (cur-cols-used
                                                          (type-pos (if (= type :offset) :width :offset))))
                                                    grid-cols))
                                (< c (cur-cols-used (type-pos type)))
                                (and (= max-cols 0)
                                     (< c (cur-cols-used (type-pos type))))
                                (and (or (= type :offset) (> c 0))
                                     (< (+ c (cur-cols-used
                                              (type-pos (if (= type :offset)
                                                          :width
                                                          :offset))))
                                        grid-cols)
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
                               (js/setTimeout #(do (check-to-hide-new-col)
                                                   (dom/remove-class! col-el :dragging))
                                              300))]
            (dom/add-class! col-el :dragging)
            (dom/remove-class! (els type) :easing)
            (dom/add-class! new-col-el :hidden)
            (dom/listen! js/document :mousemove move-handler)
            (dom/listen-once! js/document :mouseup stop-handler)))]

    (swap! layout update-in [(:pos row) :cols] conj
           {:id col-id
            :name false
            :pos (count (:cols row))
            (@settings :media-mode) [0 1]})

    (applies dom/append!
             [width-el nested-el name-el classes-el offset-handle-el]
             [col-el offset-el width-el remove-el]
             [cols-el col-el])

    (check-to-hide-new-col)
    (dom/insert-before! col-el new-col-el)
    (dom/remove-class! new-col-el :no-cols)

    (applies #(dom/listen! %1 :mousedown %2)
             [remove-el        #(handle-remove %)]
             [offset-handle-el #(handle-drag :offset %)]
             [offset-el        #(handle-drag :offset %)]
             [width-el         #(handle-drag :width %)])

    (dom/listen! name-el :change
      (fn [e]
        (let [row (get-row row-id)
              col (get-col row-id col-id)
              new-name (.-value name-el)]
          (swap! layout assoc-in [(:pos row) :cols (:pos col) :name]
                 (if (zero? (count new-name))
                   false
                   new-name)))))

    (when e (handle-drag :width e))

    [col-id col-el (go-alphabet :draw-classes draw-classes) els name-el]))

(defn create-row []
  (let [row-id (new-id! "row")
        row-el (node [:.sl-row.easing {:id (name row-id)}])
        cols-el (node [:.cols])
        name-el (node [:input.row-name {:placeholder "Name Row"}])
        tools-el (node [:.tools])
        dupe-row-el (node [:span.dupe-row [:i.icon-double-angle-down]])
        grow-row-el (node [:span.grow-row.active [:i.icon-level-down]])
        remv-row-el (node [:span.remv-row [:i.icon-remove]])
        new-col-el (node [:.new-col.no-cols])
        clear-el (node [:.clear])]

    (applies dom/append!
             [cols-el new-col-el]
             [tools-el dupe-row-el grow-row-el remv-row-el]
             [row-el cols-el name-el tools-el clear-el])

    (applies dom/listen!
             [name-el :change (fn [e] (let [row (get-row row-id)
                                           new-name (.-value name-el)]
                                       (swap! layout assoc-in [(:pos row) :name]
                                              (if (zero? (count new-name))
                                                false
                                                new-name))))]
             [new-col-el :mousedown (fn [e] (add-col! e cols-el new-col-el row-id))]
             [grow-row-el :mousedown (fn [e]
                                       (let [row (get-row row-id)]
                                         (swap! layout assoc-in [(:pos row) :wrap] true)
                                         (dom/remove-class! new-col-el :hidden)))]
             [remv-row-el :mousedown (fn [e]
               (let [row (get-row row-id)]
                 (dom/add-class! row-el :removing)
                 (dom/listen-once! row-el :transitionend
                   (fn [] (reset! layout
                                 (into []
                                       (map-indexed (fn [i r] (assoc r :pos i))
                                                    (filter (fn [r] (not (= (:id r) row-id)))
                                                            @layout))))
                     (dom/remove! row-el)))))]
             [dupe-row-el :mousedown (fn [e]
               (let [row (get-row row-id)
                     [duped-row-id duped-row-el duped-cols-el duped-new-col-el duped-name-el] (create-row)]
                 (dom/insert-after! duped-row-el row-el)
                 (reset! layout
                         (into [] (map-indexed
                                   (fn [i r] (assoc r :pos i))
                                   (insert-after @layout (inc (:pos row))
                                                 {:id duped-row-id
                                                  :pos 0
                                                  :cols []
                                                  :wrap false
                                                  :name false}))))
                 (when (:name row)
                   (let [duped-row (get-row duped-row-id)]
                     (swap! layout assoc-in [(:pos duped-row) :name] (:name row))
                     (aset duped-name-el "value" (:name row))))

                 (when (:wrap row)
                   (let [duped-row (get-row duped-row-id)]
                     (swap! layout assoc-in [(:pos duped-row) :wrap] (:wrap row))))

                 (let [new-row (get-row duped-row-id)
                       col-unit (calc-col-unit)]
                   (doseq [col (:cols row)]
                     (let [[new-col-id new-col-el new-col-in-chan els col-name-el]
                           (add-col! false duped-cols-el duped-new-col-el duped-row-id)
                           path [(:pos new-row) :cols (:pos col)]]
                       (swap! layout assoc-in (conj path :name) (:name col))
                       (dom/add-class! (:width els) :easing)
                       (when (:name col)
                         (aset col-name-el "value" (:name col)))
                       (doseq [size sizes]
                         (when (size col)
                           (applies dom/set-px!
                                    [(:offset els) :width (* col-unit ((size col) 0))]
                                    [(:width els)  :width (- (* col-unit ((size col) 1))
                                                             col-margin-width)])
                           (swap! layout assoc-in (conj path size) (size col))))
                       (when (= grid-cols (total-cols-used (get-row duped-row-id)
                                                           (@settings :media-mode)))
                         (dom/add-class! duped-new-col-el :hidden))
                       (put! new-col-in-chan [:draw-classes]))))))])
    [row-id row-el cols-el new-col-el name-el]))

(defn add-row! []
  (this-as new-row-el
    (let [[row-id row-el] (create-row)]
      (swap! layout conj {:id row-id :pos (count @layout) :cols [] :wrap false :name false})
      (dom/insert-before! row-el new-row-el))))

(defn size-classes [c]
  (remove nil?
          (flatten
           (concat
            (if (:name c) [(split (:name c) #"\s+")] [])
            (map (fn [s]
                   (if (s c)
                     (let [[offset width] (s c)]
                       [(when (> offset 0)
                          (str "col-" (name s) "-offset-" offset))
                        (str "col-" (name s) "-" width)])))
                 sizes)))))

(defn layout->html
  [rows]
  (map
   (fn [r]
     (conj (if (:name r)
             [:div.row {:class (str "row " (:name r))}]
             [:div.row])
           (map (fn [c]
                  [:div {:class (join " " (size-classes c))}])
                (:cols r))))
   rows))

(defn layout->jade
  [rows]
  (let [include-container (:include-container @settings)
        container (if include-container ".container\n" "")
        row-prefix (if include-container "  " "")
        col-prefix (if include-container "    " "  ")]
    (str container
         (->> rows
              (map
               (fn [row]
                 (str row-prefix ".row\n" col-prefix
                      (->> (:cols row)
                           (map
                            (fn [col]
                              (str "." (join "." (size-classes col)))))
                           (join (str "\n" col-prefix))))))
              (join "\n")))))

[["some-row" ["cat" [:sm 3 2] [:lg 3 3]]
             ["cat" [:sm 3 2] [:md 0 1]]]]

(def vcat (comp vec concat))

(defn layout->edn
  [rows]
  (vec (for [row rows]
         (vcat (if (:name row) [(:name row)] [])
               (for [col (:cols row)]
                 (vcat (if (:name col) [(:name col)] [])
                       (for [size sizes :when (size col)]
                         (vcat [size] (size col)))))))))

(defn make-options []
  (let [options-el (sel1 [:.options])
        ul (sel1 [:.options :ul])
        output-els {:html (sel1 :.output-html)
                    :jade (sel1 :.output-jade)
                    :haml (sel1 :.output-haml)
                    :edn  (sel1 :.output-edn)}
        els-to-mode (zipmap (vals output-els) (keys output-els))
        include-container-el (sel1 :.include-container)]

    (watch-change settings :output-mode :output-mode-done
                  (fn [old new]
                    (dom/remove-class! (output-els old) :active)
                    (dom/add-class! (output-els new) :active)))

    (dom/listen! [options-el :li]
                 :click
                 (fn [e]
                   (js/console.log e)
                   (swap! settings assoc :output-mode (els-to-mode (aget e "target")))))

    (dom/set-attr! include-container-el
                   :checked
                   (@settings :include-container))

    (dom/listen! include-container-el
                 :change
                 #(swap! settings assoc :include-container (aget include-container-el "checked")))))

(defn make-collapse-pane
  [state workspace pane-el border-el collapse-el]
  (dom/listen!
   collapse-el
   :click (fn []
            (let [collapsed (@settings state)
                  toggle-class! (if collapsed dom/remove-class! dom/add-class!)]
              (applies toggle-class!
                       [pane-el :collapsed]
                       [collapse-el :collapsed]
                       [workspace state]
                       [border-el :collapsed])
              (dom/listen-once! pane-el :transitionend
                                #(swap! settings assoc state (not collapsed)))))))

(def media-factor
  {:xs 0.02
   :sm 0.06
   :md 0.11
   :lg 0.15})

(defn make-media-previews []
  (let [preview-els (sels sizes (for [size sizes] (str ".preview." (name size) " .preview-rows")))]
    (go-alphabet
     :update #(do
                (doseq [[_ el] preview-els] (dom/set-text! el ""))
                (doseq [row @layout]
                  (doseq [[size el] preview-els]
                    (let [row-el (node [:.preview-row.easing])
                          col-unit (* col-width (media-factor size))]
                      (dom/set-px! el :width (+ (* 3 grid-cols)
                                                (* grid-cols col-unit)))
                      (dom/append! el row-el)
                      (doseq [col (:cols row)]
                        (let [col-el (node [:.preview-col.easing])
                              widths (col-for-media col size)]
                          (when widths
                            (dom/set-px! col-el :width 1)
                            (dom/append! row-el col-el)
                            (dom/set-px! col-el :width (+ (* 3 (dec (apply + widths)))
                                                          (* (apply + widths) col-unit)))))))))))))

(defn update-cols-for-media
  [media]
  (let [col-unit (calc-col-unit)]
    (doseq [row @layout]
      (doseq [col (:cols row)]
        (let [widths (col-for-media col media)
              id (id->sel (:id col))
              width-el (sel1 (str id " .width"))
              offset-el (sel1 (str id " .offset"))]
          (applies dom/set-px!
                   [offset-el :width (* col-unit (widths 0))]
                   [width-el :width (- (* col-unit (widths 1)) col-margin-width)]))))))

(def ebo [:.edn-bracket-open.tag "["])
(def ebc [:.edn-bracket-close.tag "]"])

(defn sizes->html
  [sizes]
  (vcat  [:ul.edn-media]
         (vec (for [[size offset width] sizes]
                [:li ebo
                 [:span.edn-kw.atn (str size)]
                 [:span.edn-kw.atv offset]
                 [:span.edn-kw.atv.edn-width width] ebc]))))

(defn cols->html
  [cols]
  (let [els (vcat [:ul.edn-cols]
                  (vec (for [col cols]
                         (if (string? (first col))
                           [:li ebo [:.edn-name.atv (str \" (first col) \")] (sizes->html (rest col)) ebc]
                           [:li ebo (sizes->html col) ebc]))))]
    (if (> (count els) 1)
      (assoc els
        (dec (count els))
        (conj (last els) ebc))
      (conj els ebc))))

(defn rows->html
  [rows]
  (vcat [:.all-edn ebo
         (vcat [:ul.edn-rows]
               (let [els
                     (vec (for [row rows]
                            (if (string? (first row))
                              [:li ebo [:.edn-name.atv (str \" (first row) \")] (cols->html (rest row))]
                              [:li ebo (cols->html row)])))]
                 (if (> (count els) 0)
                   (assoc els
                     (dec (count els))
                     (let [last-row (last els)
                           path [(dec (count last-row))
                                 (dec (count (last last-row)))
                                 (dec (count (last (last last-row))))]
                           last-col (get-in last-row path)]
                       (if (vector? last-col)
                         (assoc-in last-row
                                   path
                                   (conj last-col ebc))
                         (conj last-row ebc))))
                   (conj els ebc))))]))

(defn draw-workspace []
  (let [workspace (sel1 :.workspace)
        output (sel1 :pre.output)
        copy-output-el (sel1 :.copy-output)
        container (node [:.container])
        rows (node [:.rows])
        columns (node [:.columns])
        new-row (node [:.sl-row.new-row])
        media-mode (:media-mode @settings)
        media-previews-chan (make-media-previews)
        copy-code-el (sel1 :.copy-code)
        update-output (fn []
          (let [mode (:output-mode @settings)
                code (condp = mode
                       :html (let [layout-html (layout->html @layout)]
                               (js/html_beautify
                                (hrt/render-html
                                 (if (:include-container @settings)
                                   (conj [:div.container] layout-html)
                                   layout-html))))
                       :jade (layout->jade @layout)
                       :edn  (layout->edn @layout))]
            (dom/remove-class! output :prettyprinted)
            (if (= mode :edn)
              (do
                (dom/set-html! output "")
                (dom/append! output (node (rows->html code))))
              (do (dom/set-text! output (str code))
                  (js/PR.prettyPrint)))
            (aset copy-output-el "value" code)))]

    (make-options)

    (make-collapse-pane
     :medias-collapsed
     workspace
     (sel1 :.navigator)
     (sel1 :.section-border.left)
     (sel1 [:.navigator :.collapse-panel]))

    (make-collapse-pane
     :output-collapsed
     workspace
     (sel1 :.html)
     (sel1 :.section-border.right)
     (sel1 [:.html :.collapse-panel.right]))

    (dom/add-class! container media-mode)

    (doseq [i (range grid-cols)]
      (let [col (node [:.col])]
        (dom/append! columns col)))

    (doseq [size sizes]
      (let [media (sel1 (str ".preview." (name size)))]
        (when (= size media-mode) (dom/add-class! media :active))
        (dom/listen! media :mouseup #(swap! settings assoc :media-mode size))))

    (watch-change settings :media-mode
                  (fn [old-mode new-mode]
                    (applies dom/remove-class!
                             [container old-mode]
                             [(sel1 :.preview.active) :active])
                    (applies dom/add-class!
                             [(sel1 (str ".preview." (name new-mode))) :active]
                             [container new-mode])
                    (update-cols-for-media new-mode)
                    (aset workspace "scrollTop" 0)))

    (add-watch layout :update-output
               (fn [k r os ns]
                 (update-output)
                 (put! media-previews-chan [:update])))

    (watch-change settings :output-mode
                  (fn [ov nv]
                    (update-output)))

    (watch-change settings :include-container
                  (fn [ov nv]
                    (update-output)))

    (update-output)

    (dom/listen! copy-code-el :click (fn []
                                       (.select copy-output-el)
                                       (dom/listen-once! body :keyup
                                                         (fn [] (spy [:KEYUP :now-hide-popover])))))
    (dom/listen! new-row :click add-row!)
    (applies dom/append!
             [container columns rows]
             [rows new-row]
             [workspace container])))


(draw-workspace)
