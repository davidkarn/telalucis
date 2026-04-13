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
      (subs text 0 (- (count text) 1))
      (str text " "))))

(defn paragraphs-in-hocr-block
  [block]
  (loop [lines          (block-lines block)
         paragraphs     []
         this-paragraph ""
         dimensions     (get-block-dimensions block)]
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

(defn add-notes-to-paragraphs
  [main-paragraphs note-blocks main-paragraph-breaks top bottom]
  (let [line-height      (/ (- bottom top)
                            (max 1 (count main-paragraph-breaks)))
        para-line-breaks (map second
                              (filter first (map (fn [b i] [b i])
                                                 main-paragraph-breaks
                                                 (range 0 (- (count main-paragraph-breaks) 1)))))
        para-line-breaks (if (= (first para-line-breaks) 0)
                           (concat para-line-breaks [99999])
                           (concat [0] para-line-breaks [9999]))
        breaks-with-pos  (map (fn [b] [b (+ (- top line-height)
                                            (* b line-height))])
                              para-line-breaks)
        main-paragraphs  (if (= (first main-paragraphs) "")
                           (rest main-paragraphs)
                           main-paragraphs)]
    (drop-last
     1
     (map (fn [pos i]
            {:index      i
             :text       (nth main-paragraphs i 0)
             :side-notes (map
                          (fn [b] (read-single-hocr-block-text (:block b)))
                          (filter (fn [nb] (and (> (:top (:dimensions nb))
                                                   (second pos))
                                                (< (:top (:dimensions nb))
                                                   (second (nth breaks-with-pos (+ i 1))))))
                                  note-blocks))})
          breaks-with-pos
          (range 0 (count breaks-with-pos))))))
     

(defn parse-page
  [page]
  (let [blocks                (blocks-in-hocr page)
        block-sizes           (map (fn [block]
                                     (let [dimensions (get-block-dimensions block)]
                                       {:block      block
                                        :size       (* (:width dimensions) (:height dimensions))
                                        :dimensions dimensions}))
                                   blocks)
        main-block            (last (sort-by :size block-sizes))
        left-blocks           (filter (fn [b] (and
                                               (< (:right (:dimensions b))
                                                  (:left (:dimensions main-block)))
                                               (> (:top (:dimensions b))
                                                  (- (:top (:dimensions main-block)) 50))))
                                      block-sizes)
        right-blocks          (filter (fn [b] (and
                                               (> (:left (:dimensions b))
                                                  (:right (:dimensions main-block)))
                                               (> (:top (:dimensions b))
                                                  (- (:top (:dimensions main-block)) 50))))
                                      block-sizes)
        footnotes             (filter (fn [b] (and
                                               (> (:left (:dimensions b))
                                                  (:left (:dimensions main-block)))
                                               (< (:right (:dimensions b))
                                                  (:right (:dimensions main-block)))
                                               (> (:top (:dimensions b))
                                                  (:bottom (:dimensions main-block)))))
                                      block-sizes)
        main-paragraphs       (paragraphs-in-hocr-block (:block main-block))
        main-paragraph-breaks (map #(is-line-indented (:dimensions main-block) %)
                                   (block-lines (:block main-block)))]
    
    {:footnotes        (map #(read-single-hocr-block-text (:block %)) footnotes)
     :text             (add-notes-to-paragraphs main-paragraphs
                                    (concat left-blocks right-blocks)
                                    main-paragraph-breaks
                                    (:top (:dimensions main-block))
                                    (:bottom (:dimensions main-block)))
     :starts-with-para (first main-paragraph-breaks)}))

(defn parse-stbernardino-book
  []
  (let [pages (range 1 256)]))
    
