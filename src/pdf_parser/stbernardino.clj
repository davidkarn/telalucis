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
  (sel/select (sel/class "ocr_carea") contents))

(defn get-block-dimensions
  [block]
  (let [positions (map #(Integer/parseInt %)
                       (take 4 (rest (str/split (:title (:attrs block))
                                                #";? "))))]
    {:left (nth positions 0)
     :top (nth positions 1)
     :right (nth positions 2)
     :bottom (nth positions 3)
     :height (- (nth positions 3) (nth positions 1))     
     :width (- (nth positions 2) (nth positions 0))}))

(defn count-single-hocr-block-lines
  [block]
  (count (sel/select (sel/class "ocr_line") block)))

(defn block-lines
  [block]
  (sel/select (sel/class "ocr_line") block))


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

(defn is-line-indented
  [block-dimensions line] 
  (let [line-dimensions (get-block-dimensions line)
        line-char-width (/ (:width line-dimensions)
                           (count (read-single-hocr-block-text line)))]
    (>
     (- (:left line-dimensions) (:left block-dimensions))
     (/ line-char-width 2))))

(defn line-text
  [line]
  (let [text (read-single-hocr-block-text line)]
    (if (= (last text) \-)
      (subs line 0 (- (count line) 1))
      (str text " "))))

(defn paragraphs-in-hocr-block
  [block]
  (loop [lines (block-lines block)
         paragraphs []
         this-paragraph ""
         dimensions (get-block-dimensions block)]
    (cond
      (= (count lines) 0)
      (concat paragraphs [this-paragraph])

      (is-line-indented dimensions (first lines))
      (recur (rest lines)
             (concat paragraphs [this-paragraph])
             (line-text (first lines))
             dimensions)

      :else
      (recur (rest lines)
             paragraphs
             (str this-paragraph (line-text (first lines)))
             dimensions))))
    
