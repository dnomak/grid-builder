(ns shoelace.layout)

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
     :cols (map edn->col cols)
     :wrap false}))
