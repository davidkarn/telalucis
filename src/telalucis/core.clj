(ns telalucis.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [taoensso.nippy :as nippy])
  (:require [clojure.xml :as xml])
  (:require [clojure.data.json :as json])
  (:gen-class))

(def douay "/home/david/Downloads/douayr.xml")
(def vulgate "/home/david/Downloads/vulgate.xml")

(def cf-1 "/home/david/Downloads/npnf101.xml")
(def path "/home/david/projects/telalucis/public/data/")



;; ;; document-format
;; {:title "string"
;;  :id "string"
;;  :author "string"
;;  :date "string"
;;  :provenance [{:date "string" :source "string"}]
;;  :contents [{:id "string" :title "string"}]}

;; ;;section format
;; {:section-name "string"
;;  :sub-title "string"
;;  :sub-sections [<section>]
;;  :text-id "string"
;;  :languages [<string>]}

;; ;;text format
;; {:id "string" :language "string" :paragraphs [{:text <textblock>, :footnotes [<footnote>]}]}

;; ;;footnote format
;; {:num <number> :text "string" :ref <ref>}

;; ;;ref format
;; {:doc-id "string" :secs ["sec-id"... <paragraph-number>]}

;; ;;textblock-format
;; ["string"|{:dec "i"|"u"|"b" :text "string"}|{:note <number>}]

(defn sanitize-str
  [string]
  (subs (str/lower-case (str/replace string #"[^\w]" "-"))
        0
        (min (count string) 10)))

(defn get-book
  [title id book-data]
  (seek (fn [book] (and (= (:title book) title)
                        (= (:id book) id)))
        book-data))

(defn save-books-to-disk
  [book-data author]
  (map (fn [item]
         (let [bookpath (str path "books/" author "/"
                             (sanitize-str (:title item)) "-"
                             (sanitize-str (:id item)) "/")
               book (get-book (:title item) (:id item) book-data)]
           (.mkdir (java.io.File. bookpath))
           (map (fn [chapter]
                  (if (not (and (:title chapter) (:id chapter)))
                    (pprint ["error" chapter])
                    (with-open [wrtr (io/writer (str bookpath
                                                   (sanitize-str (:title chapter)) "-"
                                                   (sanitize-str (:id chapter)) ".json"))]
                      (.write wrtr (json/write-str chapter)))))
                (:children book))))
       (toc book-data)))

(defn seek
  "Returns first item from coll for which (pred item) returns true.
   Returns nil if no such item is present, or the not-found value if supplied."
  {:added  "1.9" ; note, this was never accepted into clojure core
   :static true}
  ([pred coll] (seek pred coll nil))
  ([pred coll not-found]
   (reduce (fn [_ x]
             (if (pred x)
               (reduced x)
               not-found))
           not-found coll)))

(defn parse-theology
  [file-path]
  (with-open [file (io/input-stream file-path)]
    (xml/parse file)))

(def contents (parse-theology cf-1))

(defn get-tag
  [element tag-name]
  (seek (fn [t] (= tag-name (:tag t))) (:content element)))

(defn get-attr
  [element attr-name]
  (attr-name (:attrs element)))

(defn toc
  [contents]
  (->> contents 
       (filter (fn [x] (and (= :section (:node-type x))
                            (not (#{"Title Page" "Preface" "Contents"} (:title x))))))
       (map (fn [x] (assoc x :children (toc (:children x)))))))

(defn parse-bible
  [contents]
  (parse-bible-node (get-tag contents :ThML.body)))
    
(defn parse-bible-node
  [tag]
  (map (fn [node]
         (cond
           (= :div2 (:tag node))
           {:type :book
            :title (get-attr node :title)
            :id (get-attr node :id),
            :chapters (parse-bible-node (:content node))}

           (= :div3 (:tag node))
           {:type :chapter
            :title (get-attr node :title)
            :id (get-attr node :id)
            :verses (let [verses (filter #(string? %) (:content node))]
                      (map
                       (fn [str, i] {:num i, :verse str})
                       verses
                       (range 1 (+ 1 (count verses)))))}

           (= :div1 (:tag node))
           (parse-bible-node (:content node))))
       (filter
        (fn [node] (#{:div1 :div2 :div3 :div4 :div5 :div6 :p} (:tag node)))
        (:content tag))))


(defn get-sections
  [tag]
  (map (fn [node]
         (cond
           (#{:div1 :div2 :div3 :div4 :div5 :div6} (:tag node))
           {:title       (get-attr node :title)
            :node-type   :section
            :sub-title   (get-attr node :shorttitle)
            :id          (get-attr node :id)
            :type        (get-attr node :type)
            :shorttitle  (get-attr node :shorttitle)
            :number      (get-attr node :n)
            :children    (get-sections node)}
           (= :p (:tag node))
           (process-paragraph node)))
       (filter
        (fn [node] (#{:div1 :div2 :div3 :div4 :div5 :div6 :p} (:tag node)))
        (:content tag))))

(defn get-notes
  [para]
  (filter (fn [node] (#{:scripRef :note} (:tag node)))
                        (:content para)))

(defn extract-references
  [para]
  (case (:tag para)
    :pb       nil
    :note     {:notes [] :contents {:tag :note :id (get-attr para :id)}}
    :scripRef {:notes [] :contents {:tag :note :id (get-attr para :id)}}
    :i        {:tag :i
               :contents (filter identity (map extract-references (:content para)))
               :notes (get-notes para)}
    nil       (if (string? para)
                {:notes    []
                 :contents para}
                nil)
    (let [notes    (get-notes para)
          contents (filter identity (map extract-references (:content para)))]
      {:notes    (into notes (filter identity (flatten (map :notes contents))))
       :contents (filter identity (map :contents contents))})))

(defn process-paragraph
  [node]
  (extract-references node))

(defn get-pages
  [contents]
  (get-sections (get-tag contents :ThML.body)))

(defn char-to-int
  [c]
  (case c
    \I 1    \i 1
    \V 5    \v 5
    \X 10   \x 10
    \L 50   \l 50
    \C 100  \c 100
    \D 500  \d 500
    \M 1000 \m 1000
    0))

(defn parse-roman-numeral
  [str]
  (loop [i 1
         num (char-to-int (get str 0))]
    (if (>= i (count str))
      num
      (let [pre (char-to-int (get str (- i 1)))
            cur (char-to-int (get str i))]
        (recur (+ i 1)
               (if (<= cur pre)
                 (+ num cur)
                 (+ (- num (* pre 2))
                    cur)))))))
           
(get-pages contents)
