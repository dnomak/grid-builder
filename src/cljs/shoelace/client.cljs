(ns shoelace.client
  (:require
    [dommy.core :as dom]
    [cljs.core.async :refer [>! <! chan sliding-buffer]]
    [bigsky.aui.draggable :refer [draggable]])
  (:require-macros
    [cljs.core.async.macros :refer [go]]
    [dommy.macros :refer [node]]))

(def body js/document.body)

(def layout (atom []))

(defn add-row [rows-el]
  (dom/prepend! rows-el (node [:.row])))

(defn draw-workspace []
  (let [workspace (node [:.workspace])
        medias (node [:.medias])
        media-xs (node [:.media.media-xs [:h4 "@media-xs"]])
        media-tablet (node [:.media.media-tablet [:h4 "@media-tablet"]])
        media-desktop (node [:.media.media-desktop [:h4 "@media-desktop"]])
        media-lg-desktop (node [:.media.media-lg-desktop [:h4 "@media-lg-desktop"]])
        container (node [:.container])
        rows (node [:.rows])
        columns (node [:.columns])
        cols (doall (map (fn [i]
                           (let [col (node [:.col])]
                             (dom/append! columns col)
                             col))
                         (range 13)))
        new-row (node [:.row.new-row])]
    (dom/listen! new-row :click #(add-row rows))
    (dom/append! container columns)
    (dom/append! container rows)
    (dom/append! rows new-row)
    (dom/append! medias media-xs)
    (dom/append! medias media-tablet)
    (dom/append! medias media-desktop)
    (dom/append! medias media-lg-desktop)
    (dom/append! workspace medias)
    (dom/append! workspace container)
    (dom/append! body workspace)))

(draw-workspace)
