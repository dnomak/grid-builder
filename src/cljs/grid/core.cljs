(ns grid.core
  (:require
   [clojure.string :refer [join split]]
   [cljs.reader :refer [read-string]]
   [hiccups.runtime :as hrt]))

(def vcat (comp vec concat))

(def sizes [:xs :sm :md :lg])

(def sizes-index {:xs 0 :sm 1 :md 2 :lg 3})

(defn valid-size?
  [size]
  (not (nil? (sizes-index size))))

(defn all-true?
  [bools]
  (apply (every-pred true?) bools))

(defn between?
  [n low high]
  (and (>= n low)
       (<= n high)))

(defn valid-media?
  [[size offset width]]
  (and (valid-size? size)
       (integer? offset)
       (integer? width)
       (between? offset 0 12)
       (between? width 1 12)))

(defn valid-layout-col?
  [col]
  (when (vector? col)
    (all-true?
     (map valid-media?
          (if (string? (first col))
            (rest col)
            col)))))

(defn valid-layout-row?
  [row]
  (when (vector? row)
    (all-true?
     (map valid-layout-col?
          (if (string? (first row))
            (rest row)
            row)))))

(defn valid-layout?
  [data]
  ;;[ [string? [string? [(:xs :sm :md :g) 0-12 0-12]]]]
  (and (vector? data)
       (all-true? (map valid-layout-row? data))))

(defn edn->col
  [data]
  (let [[name medias] (if (string? (first data))
                        [(first data) (rest data)]
                        [false data])]
    (apply (partial assoc {:name name})
           (apply vcat (map (fn [[s o w]] [s [o w]]) medias)))))

(defn edn->row
  [data]
  (let [[name cols] (if (string? (first data))
                      [(first data) (rest data)]
                      [false data])]
    {:name name
     :cols (map-indexed (fn [i c] (assoc c :pos i))
                        (map edn->col cols))
     :wrap false}))

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

(defn edn-string->layout
  [edn-string]
  (let [rows (read-string edn-string)]
    (when (valid-layout? rows)
      (map edn->row rows))))

(defn edn-string->html
  [edn-string]
  (hrt/render-html (layout->html (edn-string->layout edn-string))))
