(ns pdf-parser.stbernardino
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [hickory.core :as hickory])
  (:require [hickory.select :as sel])
  (:require [clojure.data.json :as json])
  (:require [telalucis.core :as core]))

(import '[org.jsoup Jsoup])

(def file-path (str core/app-root "resources/data/sermons_21.hocr"))

(defn pull-file
  [path]
  (hickory.core/as-hickory
   (hickory.core/parse
    (slurp path))))

(defn pull-page
  [i]
  (pull-file (str core/app-root "resources/data/sermons_" i ".hocr")))

(defn blocks-in-hocr
  [contents]
  (sel/select (sel/class "ocr_carea") contents))

(defn max-dimension
  [block-sizes dimension ]
  (if (= (count block-sizes) 0)
    nil
    (apply max (map #(dimension (:dimensions %)) block-sizes))))

(defn min-dimension
  [block-sizes dimension ]
  (if (= (count block-sizes) 0)
    nil
    (apply min (map #(dimension (:dimensions %)) block-sizes))))

(defn merge-split-main-text-blocks
  [block-sizes]
  (let [content-height (max-dimension block-sizes :bottom)
        content-width  (max-dimension block-sizes :right)
        biggest        (last block-sizes)]
    (if (>= (/ (- (:bottom (:dimensions biggest))
                  (:top (:dimensions biggest)))
               content-height)
            0.75)
      block-sizes
      (let [similar-blocks (filter (fn [block]
                                     (and (< (/ (abs (- (:left (:dimensions block))
                                                        (:left (:dimensions biggest))))
                                                content-width)
                                             0.05)
                                          (< (/ (abs (- (:right (:dimensions block))
                                                        (:right (:dimensions biggest))))
                                                content-width)
                                             0.05)))
                                   block-sizes)]
        (if (= (count similar-blocks) 0)
          block-sizes
          (let [matches      (group-by
                              (fn [block]
                                (and (>= (:left (:dimensions block))
                                         (- (min-dimension similar-blocks :left)
                                            (* content-width 0.05)))
                                     (<= (:right (:dimensions block))
                                         (+ (max-dimension similar-blocks :right)
                                            (* content-width 0.05)))
                                     (>= (:top (:dimensions block))
                                         (min-dimension similar-blocks :top))
                                     (<= (:bottom (:dimensions block))
                                         (max-dimension similar-blocks :bottom))))
                              block-sizes)
                not-matching (get matches false)
                matching     (sort-by :top (get matches true))
                top          (min-dimension matching :top)
                bottom       (max-dimension matching :bottom)
                left         (min-dimension matching :left)
                right        (max-dimension matching :right)]
            (concat not-matching
                    [{:dimensions {:top    top
                                   :bottom bottom
                                   :left   left
                                   :right  right
                                   :width  (- right left)
                                   :height (- bottom top)}
                      :size       (* (- right left) (- bottom top)),
                      :block      {:type    :element,
                                   :attrs
                                   {:class "ocr_carea",
                                    :id    (:id (:attrs (:block (first matching))))
                                    :title (str "bbox " left " " top " " right " " bottom)}
                                   :tag     :div
                                   :content (apply concat
                                                   (map #(:content (:block %))
                                                        matching))}}])))))))
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
  [block-dimensions line prev-line]
  (let [line-dimensions (get-block-dimensions line)
        line-char-width (/ (:width line-dimensions)
                           (count (read-single-hocr-block-text line)))]
    (>
     (- (:left line-dimensions)
        (if prev-line
          (:left (get-block-dimensions prev-line))
          (:left block-dimensions)))
     (* 2 line-char-width))))

(defn line-text
  [line]
  (let [text (read-single-hocr-block-text line)]
    (if (= (last text) \-)
      (subs text 0 (- (count text) 1))
      (str text " "))))

(defn paragraphs-in-hocr-block
  [block]
  (loop [lines          (block-lines block)
         prev-line      nil
         paragraphs     []
         this-paragraph ""
         dimensions     (get-block-dimensions block)]
    (cond
      (= (count lines) 0)
      (concat paragraphs [this-paragraph])

      (is-line-indented dimensions (first lines) prev-line)
      (recur (rest lines)
             (first lines)
             (concat paragraphs [this-paragraph])
             (line-text (first lines))
             dimensions)

      :else
      (recur (rest lines)
             (first lines)
             paragraphs
             (str this-paragraph (line-text (first lines)))
             dimensions))))

(defn add-notes-to-paragraphs
  [main-paragraphs note-blocks main-paragraph-breaks top bottom page-number]
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
            {:index       i
             :page-number page-number
             :text        (nth main-paragraphs i 0)
             :side-notes  (map
                           (fn [b] (read-single-hocr-block-text (:block b)))
                           (filter (fn [nb] (and (> (:top (:dimensions nb))
                                                    (second pos))
                                                 (< (:top (:dimensions nb))
                                                    (second (nth breaks-with-pos (+ i 1))))))
                                   note-blocks))})
          breaks-with-pos
          (range 0 (count breaks-with-pos))))))
     

(defn get-block-sizes
  [blocks]
  (sort-by :size 
           (map (fn [block]
                  (let [dimensions (get-block-dimensions block)]
                    {:block      block
                     :size       (* (:width dimensions) (:height dimensions))
                     :dimensions dimensions}))
                blocks)))

(defn parse-page
  [page page-number]
  (let [blocks                (blocks-in-hocr page)
        block-sizes           (merge-split-main-text-blocks (get-block-sizes blocks))
        main-block            (last block-sizes)
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
        main-lines            (block-lines (:block main-block))
        main-paragraph-breaks (map-indexed (fn [i line]
                                             (is-line-indented (:dimensions main-block)
                                                               line
                                                               (if (> i 0)
                                                                 (nth main-lines (- i 1))
                                                                 nil)))
                                           main-lines)]

    {:footnotes        (map #(read-single-hocr-block-text (:block %)) footnotes)
     :page-number      page-number
     :text             (add-notes-to-paragraphs main-paragraphs
                                                (concat left-blocks right-blocks)
                                                main-paragraph-breaks
                                                (:top (:dimensions main-block))
                                                (:bottom (:dimensions main-block))
                                                page-number)
     :starts-with-para (first main-paragraph-breaks)}))

(defn combine-paragraphs
  [last-para next-page-text]
  (concat [{:footnotes  (concat (or (:footnotes last-para) [])
                                (:footnotes next-page-text))
            :side-notes (concat (:side-notes last-para)
                                (:side-notes (first (:text next-page-text))))
            :text       (str (:text last-para)
                             (:text (first (:text next-page-text))))}]
          (rest (:text next-page-text))))
                                
(defn parse-stbernardino-book
  ([] (parse-stbernardino-book 8 256))
  ([start-page end-page]
   (let [pages (range start-page end-page)]
     (loop [page       start-page
            paragraphs []]
       (clojure.pprint/pprint page)
       (if (>= page (last pages))
         paragraphs
         (if (#{115 167} page)
           (recur (+ page 1) paragraphs)
           (let [page-text (parse-page (pull-page page) page)]
             (if (or (= (count paragraphs) 0)
                     (:starts-with-para page-text))
               (recur (+ page 1)
                      (concat paragraphs (concat [(assoc (first (:text page-text))
                                                         :footnotes
                                                         (:footnotes page-text))]
                                                 (rest (:text page-text)))))
               (recur (+ page 1)
                      (concat (drop-last paragraphs)
                              (combine-paragraphs (last paragraphs) page-text)))))))))))

(defn write-book
  [path]
  (with-open [wrtr (io/writer path)]
    (.write wrtr (json/write-str (parse-stbernardino-book)))))
