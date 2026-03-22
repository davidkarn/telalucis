(ns pdf-parser.stbernardino
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [hickory.core :as hickory])
  (:require [hickory.select :as sel])
  (:require [telalucis.core :as core]))

(import '[org.jsoup Jsoup])

(def file-path (str core/app-root "resources/data/sermons_21.hocr"))

(defn pull-file
  [path]
  (hickory.core/as-hickory
   (hickory.core/parse
    (slurp path))))

(defn blocks-in-hocr
  [contents]
  (sel/select (sel/class "ocr_carea") testcontents))

(defn get-block-dimensions
  [block]
  (let [positions (map #(Integer/parseInt %)
                       (rest (str/split (:title (:attrs block))
                                        #" ")))]
    {:left (nth positions 0)
     :top (nth positions 1)
     :right (nth positions 2)
     :bottom (nth positions 3)
     :height (- (nth positions 3) (nth positions 1))     
     :width (- (nth positions 2) (nth positions 0))}))

(defn count-single-hocr-block-lines
  [block]
  (count (sel/select (sel/class "ocr_line") block)))

(defn read-single-hocr-block-text
  [block]
  (str/replace
   (->> block
        
        (sel/select (sel/tag "span"))
        (map :content)
        (flatten)
        (filter (fn [c] (and (string? c)
                             (not= "" (str/trim c)))))
        (str/join " "))
   "- " "-"))


