(ns telalucis.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [taoensso.nippy :as nippy])
  (:require [clojure.xml :as xml])
  (:require [clojure.data.json :as json])
  (:require [telalucis.scripture-refs :as scrip-ref])
  (:gen-class))

(def docs-root "/Users/davidkarn/Downloads/")
(def app-root "/Users/davidkarn/Projects/telalucis/")

(def douay (str docs-root "douay.xml"))
(def vulgate (str docs-root "vulgate.xml"))

(def cf-1 (str docs-root "npnf101.xml"))
(def path (str app-root "public/data/"))

(def conf1-path (str app-root "public/data/books/augustine/the-confes-vi/he-continu-vi-xii.json"))

(defn read-chapter-from-disk
  [author book chapter]
  (json/read-str
   (slurp (str path "books/" author
               "/" book
               "/" chapter ".json"))
   :key-fn keyword))

(defn note-has-scripture-ref
  [note]
  (if (or (= (:tag note) "scripRef")
          (= (:tag note) :scripRef))
    note
    (if (:content note)
      (first (filter identity (map note-has-scripture-ref (:content note))))
      nil)))

(defn chapter-notes
  [chapter-data author book chapter-name book-id chapter-id]
  (filter
   identity
   (flatten
    (map (fn [para]
           (let [notes-with-ref (filter note-has-scripture-ref (:notes para))]
             (map (fn [root-ref]
                    (let [scrip-ref (note-has-scripture-ref root-ref)]
                      (if scrip-ref
                        {:ref        scrip-ref
                         :fn-id      (:id (:attrs root-ref))
                         :author     author
                         :book       book
                         :chapter    chapter-name
                         :book-id    book-id
                         :chapter-id chapter-id
                         :para       (flatten
                                      (filter #(not (and (= (:tag %) "note")
                                                         (not (= (:id %) (:id (:attrs root-ref))))))
                                              (:contents para)))}
                        nil)))
                  notes-with-ref)))
         (flatten (map :children (:children chapter-data)))))))

(defn path-for-scripture-ref
  [script-path]
  (str app-root "public/data/bible/refs/"
       (name (:book script-path))
       "-"
       (:chapter script-path)
       ".json"))

(defn scripture-ref-to-path
;todo - all scrip refs are not parsed, implement parser for these  
; - skip if ref cannot be parsed
; - also include volume number in book ids when saving books to disk
  [ref]
  (if (:parsed (:attrs (:ref ref)))
    (let [[_ book chapter verse] (str/split (:parsed (:attrs (:ref ref))) #"\|")]
      (clojure.pprint/pprint [book chapter verse (:parsed (:attrs (:ref ref)))])
      
      {:book    (scrip-ref/translate-book book)
       :chapter chapter
       :verse   verse})
    (let [parsed (scrip-ref/parse-ref (first (:content (:ref ref))) true)]
      (or parsed nil))))
      

(defn compile-refs
  [refs]
  (reduce (fn [table ref]
            (let [scrip-path (scripture-ref-to-path ref)
                  key        (and scrip-path
                                  (str/join ":" [(:book scrip-path)
                                                 (:chapter scrip-path)]))]
              (if (not scrip-path)
                table
                (assoc table
                       key
                       (if (get key table)
                         (assoc (get key table)
                                :refs
                              (concat ref (:refs (get key table))))
                         (assoc scrip-path :refs [ref]))))))
          {}
          refs))

(defn write-refs-to-disk
  [refs]
  (let [table (compile-refs refs)]
    (map (fn [key]
           (with-open [wrtr (io/writer (path-for-scripture-ref (get table key)))]
             (.write wrtr (json/write-str (get table key)))))
         (keys table))))

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

(defn seek-result
  "Returns the result of calling pred on the first item from coll for which (pred item) returns true.
   Returns nil if no such item is present, or the not-found value if supplied."
  {:added  "1.9" ; note, this was never accepted into clojure core
   :static true}
  ([pred coll] (seek-result pred coll nil))
  ([pred coll not-found]
   (reduce (fn [_ x]
             (let [res (pred x)]
               (if res
                 (reduced res)
                 not-found)))
           not-found
           coll)))

(defn sanitize-str
  [string]
  (cond (= string "Ecclesiasticus") ; prevent collision between Ecelesiastes and Ecclesiasticus
        "sirach"

        true
        (subs (str/lower-case (str/replace string #"[^\w]" "-"))
              0
              (min (count string) 10))))

(defn get-book
  [title id book-data]
  (seek (fn [book] (and (= (:title book) title)
                        (= (:id book) id)))
        book-data))

(defn toc
  [contents]
  (->> contents
       (filter (fn [x] (and (= :section (:node-type x))
                            (not (#{"Title Page" "Preface" "Contents"} (:title x))))))
       (map (fn [x] (assoc x :children (toc (:children x)))))))

(defn save-books-to-disk
  [book-data author]
  (map (fn [item]
         (let [bookpath (str path "books/" author "/"
                             (sanitize-str (:title item)) "-"
                             (sanitize-str (:id item)) "/")
               book     (get-book (:title item) (:id item) book-data)]
           (.mkdir (java.io.File. (str path "books/" author)))
           (.mkdir (java.io.File. bookpath))
           (map (fn [chapter]
                  (if (not (and (:title chapter) (:id chapter)))
                    (with-open [wrtr (io/writer (str bookpath
                                                     (sanitize-str (:title chapter)) "-"
                                                     (sanitize-str (:id chapter)) ".json"))]
                      (.write wrtr (json/write-str chapter)))))
                (:children book))))
       (toc book-data)))

(defn save-bible-to-disk
  [book-data translation]
  (map (fn [book]
         (.mkdir (java.io.File. (str path "bible/" translation)))
         (with-open [wrtr (io/writer (str path "bible/" translation "/"
                                          (sanitize-str (:title book)) ".json"))]
           (.write wrtr (json/write-str book))))
       (flatten (filter #(not (empty? %)) book-data))))

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

(defn parse-bible-node
  [tag]
  (map (fn [node]
         (cond
           (= :div2 (:tag node))
           {:type     :book
            :title    (get-attr node :title)
            :id       (get-attr node :id),
            :chapters (parse-bible-node node)}

           (= :div3 (:tag node))
           {:type   :chapter
            :title  (get-attr node :title)
            :id     (get-attr node :id)
            :verses (let [verses (filter #(string? %)
                                         (flatten (map :content (:content node))))]
                      (map
                       (fn [str, i] {:num i, :verse (str/trim str)})
                       verses
                       (range 1 (+ 1 (count verses)))))}

           (= :div1 (:tag node))
           (parse-bible-node node)))
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
    :i        {:tag      :i
               :contents (filter identity (map extract-references (:content para)))
               :notes    (get-notes para)}
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

(defn parse-bible
  [contents]
  (parse-bible-node (get-tag contents :ThML.body)))

(defn get-sections
  [tag]
  (map (fn [node]
         (cond
           (#{:div1 :div2 :div3 :div4 :div5 :div6} (:tag node))
           {:title      (get-attr node :title)
            :node-type  :section
            :sub-title  (get-attr node :shorttitle)
            :id         (get-attr node :id)
            :type       (get-attr node :type)
            :shorttitle (get-attr node :shorttitle)
            :number     (get-attr node :n)
            :children   (get-sections node)}
           (= :p (:tag node))
           (process-paragraph node)))
       (filter
        (fn [node] (#{:div1 :div2 :div3 :div4 :div5 :div6 :p} (:tag node)))
        (:content tag))))

(defn get-pages
  [contents]
  (get-sections (get-tag contents :ThML.body)))

(def anfs (map (fn [i] (str "anf0" i ".xml")) (range 1 10)))
(def pnfs (map (fn [i] (str "npnf" i ".xml")) (concat (range 101 115) (range 201 215))))

(defn file-toc
  [filename]
  (toc (get-pages (parse-theology (str docs-root filename)))))

(defn toc-outline
  ([toc] (toc-outline toc 2))
  ([toc level]
   (map (fn [item]
          (let [response {:title (:title item)
                          :id    (:id item)
                          :type  (:type item)}]
            (if (= level 0)
              response
              (assoc response
                     :children
                     (toc-outline (:children item)
                                  (- level 1))))))
        toc)))

(def outlines
  [(toc-outline (file-toc (nth anfs 0)) 1)
   (toc-outline (file-toc (nth anfs 1)) 1)
   (toc-outline (file-toc (nth anfs 2)) 1)
   (toc-outline (file-toc (nth anfs 3)) 1)
   (toc-outline (file-toc (nth anfs 4)) 1)
   (toc-outline (file-toc (nth anfs 5)) 1)
   (toc-outline (file-toc (nth anfs 6)) 1)
   (toc-outline (file-toc (nth anfs 7)) 1)
   (toc-outline (file-toc (nth anfs 8)) 1)])

(def cf-2 (str docs-root "npnf102.xml"))
(def cf-3 (str docs-root "npnf103.xml"))
(def cf-4 (str docs-root "npnf104.xml"))
(def cf-5 (str docs-root "npnf105.xml"))
(def cf-6 (str docs-root "npnf106.xml"))
;(save-books-to-disk (get-pages (parse-theology cf-2)) "augustine")
;(save-books-to-disk (get-pages (parse-theology cf-3)) "augustine")
;(save-books-to-disk (get-pages (parse-theology cf-4)) "augustine")
;(save-books-to-disk (get-pages (parse-theology cf-5)) "augustine")

;(get-pages contents)
;(save-bible-to-disk (parse-bible (parse-theology douay)) "douay")
;(save-books-to-disk (get-pages contents) "augustine")
;(def chapt (read-chapter-from-disk "augustine" "the-confes-vi" "he-continu-vi-xii"))
;(chapter-notes chapt "augustine" "confess" "confess-vi")
;(write-refs-to-disk (chapter-notes chapt "augustine" "confess" "confess-vi"))

(defn volume-filename
  [volume]
  (cond (< volume 20) (str docs-root "anf" (format "%02d" volume) ".xml")
        (> volume 20) (str docs-root "npnf" (format "%03d" volume) ".xml")))

(defn get-volume
  [volume]
  (get-pages (parse-theology (volume-filename volume))))

(defn get-volume-for-refs
  [volume]
  (parse-theology (volume-filename volume)))

(defn get-book-by-id
  [volume id]
  (cond (= (:id volume) id)          volume
        (= (:id (:attrs volume)) id) volume
        (seq? volume)                (seek-result #(get-book-by-id % id)
                                                  volume)
        (:children volume)           (seek-result #(get-book-by-id % id)
                                                  (:children volume))
        (:content volume)            (seek-result #(get-book-by-id % id)
                                                  (:content volume))
        true                         false))

(defn save-book-to-disk
  [author title id book-data]
  (map (fn [chapter]
         (.mkdir (java.io.File. (str path "books/" author)))
         (.mkdir (java.io.File. (str path "books/" author "/" id)))
         (with-open [wrtr (io/writer (str path "books/" author "/" id "/"
                                          (:id chapter) ".json"))]
           (.write wrtr (json/write-str (get-book-by-id book-data (:id chapter))))))
       (toc [book-data])))

(defn write-ref
  [ref]
  (let [path          (path-for-scripture-ref ref)
        file-exists   (.exists (io/file path))
        existing-data (if file-exists
                        (json/read-str (slurp path))
                        [])]
    (with-open [wrtr (io/writer path)]
      (.write wrtr (json/write-str (conj existing-data ref))))))

(defn save-book-refs-to-disk
  [author title id book-data]
  (map (fn [chapter]
         (let [refs-table (compile-refs
                           (chapter-notes (get-book-by-id book-data (:id chapter))
                                          author
                                          title
                                          (:title chapter)
                                          id
                                          (:id chapter)))]
           (clojure.pprint/pprint ["saving refs" author id (count refs-table)])
           (map (fn [key] (write-ref (get refs-table key)))
                (keys refs-table))))
       (:children (first (toc [book-data])))))
                       
(defn parse-and-save-book
  [author title id volume]
  (let [global-id (str volume ":" id)
        book-data (get-book-by-id (get-volume volume)
                                  id)]
    (save-book-to-disk author
                       title
                       global-id
                       book-data)
    (save-book-refs-to-disk author
                            title
                            global-id
                            book-data)))

(defn parse-anpn-contents
  [contents]
  (map (fn [author-details]
         (let [author (:author author-details)
               volume (:volume author-details)]
           (clojure.pprint/pprint ["parsing" author volume])
           (map (fn [book]
                  (let [title (:title book)
                        id (:id book)
                        volume (or (:volume book) volume)]
                    (parse-and-save-book author title id volume)))
                (:books author-details))))
       contents))

(def anti-nicene-contents
  [{:author "Clement Of Rome",
    :id "ii",
    :volume 1
    :books [{:title "First Epistle to the Corinthians",
             :id "ii.ii"}]}
   {:author "Mathetes",
    :id "iii",
    :volume 1
    :books [{:title "Epistle to Diognetus", :id "iii.ii"}]}
   {:author "Polycarp",
    :id "iv",
    :volume 1
    :books [{:title "Epistle to the Philippians", :id "iv.ii"}
            {:title "The Martyrdom of Polycarp", :id "iv.iv"}]}
   {:author "Ignatius",
    :id "v",
    :volume 1
    :books [{:title "Epistle to the Ephesians: Shorter and Longer Versions",
             :id "v.ii",
             :type nil}
            {:title "Epistle to the Magnesians: Shorter and Longer Versions",
             :id "v.iii",
             :type nil}
            {:title "Epistle to the Trallians: Shorter and Longer Versions",
             :id "v.iv",
             :type nil}
            {:title "Epistle to the Romans: Shorter and Longer Versions",
             :id "v.v",
             :type nil}
            {:title
             "Epistle to the Philadelphians: Shorter and Longer Versions",
             :id "v.vi",
             :type nil}
            {:title "Epistle to the Smyrnæans: Shorter and Longer Versions",
             :id "v.vii",
             :type nil}
            {:title "Epistle to Polycarp: Shorter and Longer Versions",
             :id "v.viii",
             :type nil}
            {:title "Epistle to Polycarp: Syriac Version",
             :id "v.x",
             :type nil}
            {:title "Epistle to the Ephesians: Syriac Version",
             :id "v.xi",
             :type nil}
            {:title "Epistle to the Romans: Syriac Version",
             :id "v.xii",
             :type nil}
            {:title "Epistle to the Tarsians", :id "v.xiv", :type nil}
            {:title "Epistle to the Antiochians", :id "v.xv", :type nil}
            {:title "Epistle to Hero, a Deacon of Antioch",
             :id "v.xvi",
             :type nil}
            {:title "Epistle to the Philippians", :id "v.xvii", :type nil}
            {:title "Epistle from Maria of Cassobelæ",
             :id "v.xviii",
             :type nil}
            {:title "Epistle to Mary at Neapolis", :id "v.xix", :type nil}
            {:title "First Epistle to St John", :id "v.xx", :type nil}
            {:title "Second Epistle to St John", :id "v.xxi", :type nil}
            {:title "Epistle to Mary the Virgin", :id "v.xxii", :type nil}
            {:title "Epistle from Mary the Virgin", :id "v.xxiii", :type nil}
            {:title "Introductory Note to the Martyrdom of Ignatius",
             :id "v.xxiv",
             :type nil}
            {:title "The Martyrdom of Ignatius", :id "v.xxv", :type nil}]}
   {:author "Barnabas",
    :id "vi",
    :volume 1
    :books [{:title "The Epistle of Barnabas", :id "vi.ii", :type nil}]}
   {:author "Papias",
    :id "vii",
    :volume 1
    :books [{:title "Fragments", :id "vii.ii", :type nil}]}
   {:author "Justin Martyr",
    :id "viii",
    :volume 1
    :books [{:title "The First Apology", :id "viii.ii", :type nil}
            {:title "The Second Apology", :id "viii.iii", :type nil}
            {:title "Dialogue with Trypho", :id "viii.iv", :type nil}
            {:title "The Discourse to the Greeks", :id "viii.v", :type nil}
            {:title "Hortatory Address to the Greeks",
             :id "viii.vi",
             :type nil}
            {:title "On the Sole Government of God",
             :id "viii.vii",
             :type nil}
            {:title "On the Resurrection, Fragments",
             :id "viii.viii",
             :type nil}
            {:title "Other Fragments from the Lost Writings of Justin",
             :id "viii.ix",
             :type nil}
            {:title "The Martyrdom of Justin Martyr",
             :id "viii.xi",
             :type nil}]}
   {:author "Irenæus",
    :id "ix",
    :volume 1
    :books [{:title "Against Heresies: Book I", :id "ix.ii", :type nil}
            {:title "Against Heresies: Book II", :id "ix.iii", :type nil}
            {:title "Against Heresies: Book III", :id "ix.iv", :type nil}
            {:title "Elucidation", :id "ix.v", :type nil}
            {:title "Against Heresies: Book IV", :id "ix.vi", :type nil}
            {:title "Against Heresies: Book V", :id "ix.vii", :type nil}
            {:title "Fragments from the Lost Writings of Irenæus",
             :id "ix.viii",
             :type nil}]}
   {:author "The Pastor Of Hermas",
    :id "ii",
    :volume 2
    :books [{:title "Book First.—Visions", :id "ii.ii", :type nil}
            {:title "Book Second.—Commandments", :id "ii.iii", :type nil}
            {:title "Book Third.—Similitudes", :id "ii.iv", :type nil}
            {:title "Elucidations", :id "ii.v", :type nil}]}
   {:author "Tatian",
    :id "iii",
    :volume 2
    :books [{:title "Address to the Greeks", :id "iii.ii", :type nil}
            {:title "Fragments", :id "iii.iii", :type nil}]}
   {:author "Theophilus",
    :id "iv",
    :volume 2
    :books [{:title "Theophilus to Autolycus", :id "iv.ii", :type nil}]}
   {:author "Athenagoras",
    :id "v",
    :volume 2
    :books [{:title "A Plea for the Christians", :id "v.ii", :type nil}
            {:title "The Resurrection of the Dead", :id "v.iii", :type nil}]}
   {:author "Clement Of Alexandria",
    :id "vi",
    :volume 2
    :books [{:title "Exhortation to the Heathen", :id "vi.ii", :type nil}
            {:title "The Instructor", :id "vi.iii", :type nil}
            {:title "The Stromata, or Miscellanies", :id "vi.iv", :type nil}
            {:title "Who is the Rich Man that shall be saved?",
             :id "vi.v",
             :type nil}]}
   {:author "Tertullian"
    :books [{:title "Apology", :id "iv.iii", :type nil :volume 3}
            {:title "On Idolatry", :id "iv.iv", :type nil :volume 3}
            {:title "The Shows, or De Spectaculis", :id "iv.v", :type nil :volume 3}
            {:title "The Chaplet, or De Corona", :id "iv.vi", :type nil :volume 3}
            {:title "To Scapula", :id "iv.vii", :type nil :volume 3}
            {:title "Ad Nationes", :id "iv.viii", :type nil :volume 3}
            {:title "An Answer to the Jews", :id "iv.ix", :type nil :volume 3}
            {:title "The Soul's Testimony", :id "iv.x", :type nil :volume 3}
            {:title "A Treatise on the Soul", :id "iv.xi", :type nil :volume 3}
            {:title "Anti-Marcion" :id "v" :volume 3}
            {:title "On Repentance", :id "vi.ii", :type nil :volume 3}
            {:title "On Baptism", :id "vi.iii", :type nil :volume 3}
            {:title "On Prayer", :id "vi.iv", :type nil :volume 3}
            {:title "Ad Martyras", :id "vi.v", :type nil :volume 3}
            {:title
             "The Passion of the Holy Martyrs Perpetua and Felicitas",
             :id "vi.vi",
             :type nil :volume 3}
            {:title "On Patience", :id "vi.vii", :type nil :volume 3}
            {:title "On the Pallium", :id "iii.ii", :type nil :volume 4}
            {:title "On the Apparel of Women", :id "iii.iii", :type nil :volume 4}
            {:title "On the Veiling of Virgins", :id "iii.iv", :type nil :volume 4}
            {:title "To His Wife", :id "iii.v", :type nil :volume 4}
            {:title "On Exhortation to Chastity", :id "iii.vi", :type nil :volume 4}
            {:title "On Monogamy", :id "iii.vii", :type nil :volume 4}
            {:title "On Modesty", :id "iii.viii", :type nil :volume 4}
            {:title "On Fasting", :id "iii.ix", :type nil :volume 4}
            {:title "De Fuga in Persecutione", :id "iii.x", :type nil :volume 4}]}
   {:author "Minucius Felix",
    :id "iv",
    :volume 4
    :books [{:title "The Octavius of Minucius Felix",
             :id "iv.iii",
             :type nil}]}
   {:author "Commodianus",
    :id "v",
    :volume 4
    :books [{:title "The Instructions of Commodianus",
             :id "v.ii",
             :type nil}]}
   {:author "Origen",
    :id "vi",
    :volume 4
    :books [{:title "Prologue of Rufinus", :id "vi.iv", :type nil}
            {:title "Origen De Principiis", :id "vi.v", :type nil}
            {:title
             "A Letter to Origen from Africanus About the History of Susanna",
             :id "vi.vi",
             :type nil}
            {:title "A Letter from Origen to Africanus",
             :id "vi.vii",
             :type nil}
            {:title "A Letter from Origen to Gregory",
             :id "vi.viii",
             :type nil}
            {:title "Origen Against Celsus", :id "vi.ix", :type nil}
            {:title "Origen's Commentary on the Gospel of John",
             :id "xv.iii",
             :volume 9}
            {:title "Origen's Commentary on Matthew",
             :id "xvi.ii",
             :volume 9}]}
   {:author "Hippolytus",
    :id "iii",
    :volume 5
    :books [{:title "The Refutation of All Heresies",
             :id "iii.iii",
             :type nil}
            {:title "The Extant Works and Fragments of Hippolytus",
             :id "iii.iv",
             :type nil}
            {:title
             "Appendix to the Works of Hippolytus. Containing Dubious and Spurious Pieces",
             :id "iii.v",
             :type nil}]}
   {:author "Cyprian",
    :id "iv",
    :volume 5
    :boooks [{:title
              "The Life and Passion of Cyprian, Bishop and Martyr. By Pontius the Deacon",
              :id "iv.iii",
              :type nil}
             {:title "The Epistles of Cyprian", :id "iv.iv", :type nil}
             {:title "The Treatises of Cyprian", :id "iv.v", :type nil}
             {:title
              "The Seventh Council of Carthage under Cyprian. Concerning the Baptism of Heretics",
              :id "iv.vi",
              :type nil}
             {:title
              "Treatises Attributed to Cyprian on Questionable Authority",
              :id "iv.vii",
              :type nil}]}
   {:author "Caius",
    :id "v",
    :volume 5
    :books [{:title "Fragments of Caius", :id "v.iii", :type nil}]}
   {:author "Novatian",
    :id "vi",
    :volume 5
    :books [{:title "A Treatise of Novatian Concerning the Trinity",
             :id "vi.iii",
             :type nil}
            {:title "On the Jewish Meats", :id "vi.iv", :type nil}]}
   {:author "Gregory Thaumaturgus",
    :id "iii",
    :volume 6
    :books [{:title "Acknowledged Writings", :id "iii.iii", :type "Part"}
            {:title "Dubious or Spurious Writings",
             :id "iii.iv",
             :type "Part"}]}
   {:author "Dionysius",
    :id "iv",
    :volume 6
    :books [{:title "Extant Fragments", :id "iv.iii", :type nil}
            {:title "Exegetical Fragments", :id "iv.iv", :type nil}]}
   {:author "Julius Africanus",
    :id "v",
    :volume 6
    :books [{:title "The Epistle to Aristides",
             :id "v.iii",
             :type "Section"}
            {:title
             "Narrative of Events Happening in Persia on the Birth of Christ",
             :id "v.iv",
             :type "Section"}
            {:title
             "The Extant Fragments of the Five Books of the Chronography of Julius Africanus",
             :id "v.v",
             :type "Section"}
            {:title "The Passion of St. Symphorosa and Her Seven Sons",
             :id "v.vi",
             :type "Section"}
            {:title "Elucidations", :id "v.vii", :type nil}]}
   {:author "Anatolius Of Alexandria",
    :id "vi.iii",
    :volume 6
    :books [{:title "The Paschal Canon of Anatolius of Alexandria",
             :id "vi.iii.ii",
             :type nil}
            {:title "Fragments of the Books on Arithmetic",
             :id "vi.iii.iii",
             :type nil}]}
   {:author "Alexander Of Cappadocia",
    :id "vi.iv",
    :volume 6
    :books [{:title "From the Epistles of Alexander",
             :id "vi.iv.ii",
             :type nil}]}
   {:author "Theognostus Of Alexandria",
    :id "vi.v",
    :volume 6
    :books [{:title "From His Seven Books of Hypotyposes or Outlines",
             :id "vi.v.ii",
             :type nil}]}
   {:author "Pierus Of Alexandria",
    :id "vi.vi",
    :volume 6
    :books [{:title
             "A Fragment of a Work of Pierius on the First Epistle of Paul to the Corinthians",
             :id "vi.vi.ii",
             :type "Section"}
            {:title "A Section on the Writings of Pierius",
             :id "vi.vi.iii",
             :type "Section"}]}
   {:author "Theonas Of Alexandria",
    :id "vi.vii",
    :volume 6
    :books [{:title
             "The Epistle of Theonas, Bishop of Alexandria, to Lucianus, the Chief Chamberlain",
             :id "vi.vii.ii",
             :type nil}]}
   {:author "Phileas",
    :id "vi.viii",
    :volume 6
    :books [{:title
             "Fragments of the Epistle of Phileas to the People of Thmuis",
             :id "vi.viii.ii",
             :type nil}
            {:title
             "The Epistle of the Same Phileas of Thmuis to Meletius, Bishop of Lycopolis",
             :id "vi.viii.iii",
             :type nil}]}
   {:author "Pamphilus",
    :id "vi.ix",
    :volume 6
    :books [{:title
             "An Exposition of the Chapters of the Acts of the Apostles",
             :id "vi.ix.ii",
             :type nil}]}
   {:author "Malchion",
    :id "vi.x",
    :volume 6
    :books [{:title
             "The Epistle Written by Malchion, In Name of the Synod of Antioch, Against Paul of Samosata"
             :id "vi.x.ii",
             :type "Section"}
            {:title
             "Fragments Apparently of the Same Epistle of the Synod of Antioch",
             :id "vi.x.iii",
             :type "Section"}
            {:title
             "From the Acts of the Disputation Conducted by Malchion Against Paul of Samosata",
             :id "vi.x.iv",
             :type "Section"}
            {:title "A Point in the Same Disputation",
             :id "vi.x.v",
             :type "Section"}
            {:title "Elucidations", :id "vi.x.vi", :type nil}]}
   {:author "Archelaus",
    :id "vii",
    :volume 6
    :books [{:title "The Acts of the Disputation with the Heresiarch Manes",
             :id "vii.iii",
             :type nil}
            {:title "A Fragment of the Same Disputation",
             :id "vii.iv",
             :type nil}]}
   {:author "Alexander Of Lycopolis",
    :id "viii",
    :volume 6
    :books [{:title "Of the Manichæans", :id "viii.iii", :type nil}
            {:title "Elucidation", :id "viii.iv", :type nil}]}
   {:author "Peter Of Alexandria",
    :id "ix",
    :volume 6
    :books [{:title "The Genuine Acts of Peter", :id "ix.iii", :type nil}
            {:title
             "The Canonical Epistle, with the Commentaries of Theodore Balsamon and John Zonaras",
             :id "ix.iv",
             :type nil}
            {:title "Fragments from the Writings of Peter",
             :id "ix.vi",
             :type nil}]}

   {:author "Alexander Of Alexandria",
    :id "x",
    :volume 6
    :books [{:title
             "Epistles on the Arian Heresy and the Deposition of Arius",
             :id "x.iii",
             :type nil}]}
   {:author "Methodius",
    :id "xi",
    :volume 6
    :books [{:title
             "The Banquet of the Ten Virgins; or Concerning Chastity",
             :id "xi.iii",
             :type nil}
            {:title "Concerning Free-Will", :id "xi.iv", :type nil}
            {:title "From the Discourse on the Resurrection",
             :id "xi.v",
             :type nil}
            {:title "Fragments", :id "xi.vi", :type nil}
            {:title
             "Oration Concerning Simeon and Anna On the Day that They Met in the Temple",
             :id "xi.viii",
             :type nil}
            {:title "Oration on the Palms", :id "xi.ix", :type nil}
            {:title
             "Three Fragments from the Homily on the Cross and Passion of Christ",
             :id "xi.x",
             :type nil}
            {:title "Some Other Fragments of the Same Methodius",
             :id "xi.xi",
             :type nil}
            {:title "Two Fragments, Uncertain", :id "xi.xii", :type nil}]}
   {:author "Arnobius",
    :id "xii",
    :volume 6
    :books [{:title
             "The Seven Books of Arnobius Against the Heathen. (Adversus Gentes.)",
             :id "xii.iii",
             :type nil}]}
   {:author "Lactantius",
    :id "iii",
    :volume 7
    :books [{:title "The Divine Institutes", :id "iii.ii", :type nil}
            {:title "A Treatise on the Anger of God Addressed to Donatus",
             :id "iii.iii",
             :type nil}
            {:title "On the Workmanship of God, or the Formation of Man",
             :id "iii.iv",
             :type nil}
            {:title "Of the Manner in Which the Persecutors Died",
             :id "iii.v",
             :type nil}
            {:title "Fragments of Lactantius", :id "iii.vi", :type nil}]}

   {:author "Venantius ",
    :id "iv",
    :volume 7
    :books [{:title "On Easter", :id "iv.i", :type nil}]}
   {:author "Asterius Urbanus",
    :id "v",
    :volume 7
    :books [{:title "The Extant Writings of Asterius Urbanus",
             :id "v.ii",
             :type nil}]}
   {:author "Victorinus",
    :id "vi",
    :volume 7
    :books [{:title "On the Creation of the World", :id "vi.i", :type nil}
            {:title "Commentary on the Apocalypse of the Blessed John",
             :id "vi.ii",
             :type nil}]}
   {:author "Dionysius",
    :id "vii",
    :volume 7
    :books [{:title "Against the Sabellians", :id "vii.ii", :type nil}]}])

(def post-nicene-contents
  [

   {:author "Augustine",
    :books    [{:title "The Confessions",
          :id        "vi",
          :volume    101}
          {:title  "Letters of St. Augustin",
           :id     "vii",
           :volume 101}
          {:title  "City of God"
           :volume 102
           :id     "iv"}
          {:title  "On Christian Doctrine",
           :id     "v",
           :volume 102}
          {:title "On the Holy Trinity.", :id "iv.i", :volume 103}
          {:title "The Enchiridion.", :id "iv.ii", :volume 103}
          {:title  "On the Catechising of the Uninstructed.",
           :id     "iv.iii",
           :volume 103}
          {:title  "A Treatise on Faith and the Creed.",
           :id     "iv.iv",
           :volume 103}
          {:title  "Concerning Faith of Things Not Seen.",
           :id     "iv.v",
           :volume 103}
          {:title "On the Profit of Believing.", :id "iv.vi", :volume 103}
          {:title "On the Creed.", :id "iv.vii", :volume 103}
          {:title "On Continence.", :id "v.i", :volume 103}
          {:title "On the Good of Marriage.", :id "v.ii", :volume 103}
          {:title "Of Holy Virginity.", :id "v.iii", :volume 103}
          {:title "On the Good of Widowhood.", :id "v.iv", :volume 103}
          {:title "On Lying.", :id "v.v", :volume 103}
          {:title "Against Lying.", :id "v.vi", :volume 103}
          {:title "Of the Work of Monks.", :id "v.vii", :volume 103}
          {:title "On Patience.", :id "v.viii", :volume 103}
          {:title "On Care to Be Had for the Dead.",
           :id "v.ix",
           :volume 103}
          {:title "Introductory Essay on the Manichæan Heresy.",
           :id "iv.ii",
           :volume 104}
          {:title "Preface to the Anti-Manichæan Writings.",
           :id "iv.iii",
           :volume 104}
          {:title "On the Morals of the Catholic Church.",
           :id "iv.iv",
           :volume 104}
          {:title "On the Morals of the Manichæans.",
           :id "iv.v",
           :volume 104}
          {:title "On Two Souls, Against the Manichæans.",
           :id "iv.vi",
           :volume 104}
          {:title "Acts or Disputation Against Fortunatus the Manichæan.",
           :id "iv.vii",
           :volume 104}
          {:title " Against the Epistle of Manichæus, Called Fundamental.",
           :id "iv.viii",
           :volume 104}
          {:title "Reply to Faustus the Manichæan.",
           :id "iv.ix",
           :volume 104}
          {:title "Concerning the Nature of Good, Against the Manichæans.",
           :id "iv.x",
           :volume 104}
          {:title "On Baptism, Against the Donatists.",
           :id "v.iv",
           :volume 104}
          {:title "Answer to the Letters of Petilian, the Donatist.",
           :id "v.v",
           :volume 104}
          {:title "The Correction of the Donatists.",
           :id "v.vi",
           :volume 104}
          {:title
           "A Treatise on the Merits and Forgiveness of Sins, and on the Baptism of Infants.",
           :id "x",
           :volume 105},
          {:title "A Treatise on the Spirit and the Letter.",
           :id "xi",
           :volume 105}
          {:title "A Treatise on Nature and Grace.",
           :id "xii",
           :volume 105}
          {:title "A Treatise Concerning Man’s Perfection in Righteousness.",
           :id "xiii",
           :volume 105}
          {:title "A Work on the Proceedings of Pelagius.",
           :id "xiv",
           :volume 105}
          {:title "A Treatise on the Grace of Christ, and on Original Sin.",
           :id "xv",
           :volume 105}
          {:title "On Marriage and Concupiscence.",
           :id "xvi",
           :volume 105}
          {:title "A Treatise on the Soul and its Origin.",
           :id "xvii",
           :volume 105}
          {:title "A Treatise Against Two Letters of the Pelagians.",
           :id "xviii",
           :volume 105}
          {:title "A Treatise on Grace and Free Will.",
           :id "xix",
           :volume 105}
          {:title "A Treatise on Rebuke and Grace.",
           :id "xx",
           :volume 105}
          {:title "A Treatise on the Predestination of the Saints.",
           :id "xxi",
           :volume 105}
          {:title "Our Lord’s Sermon on the Mount.",
           :id "v",
           :volume 106}
          {:title "The Harmony of the Gospels.",
           :id "vi",
           :volume 106}
          {:title "Sermons on Selected Lessons of the New Testament.",
           :id "vii",
           :volume 106}
          {:title
           "Lectures or Tractates on the Gospel According to St. John.",
           :id "iii",
           :volume 107}
          {:title "Ten Homilies on the First Epistle of John.",
           :id "iv",
           :volume 107}
          {:title "Two Books of Soliloquies.",
           :id "v",
           :volume 107}
          {:title "Expositions on the Book of Psalms.",
           :id "ii",
           :volume 108}]}
 {:author "St. John Crysostom",
  :books [{:title "Prolegomena.",
           :id "iii",
           :volume 109}
          {:title "Treatise Concerning the Christian Priesthood.",
           :id "iv",
           :volume 109}
          {:title "An Exhortation to Theodore After His Fall."
           :id "v"
           :volume 109}
          {:title "Letter to a Young Widow.",
           :id "vi",
           :volume 109}
          {:title "Homilies on S. Ignatius and S. Babylas.",
           :id "vii",
           :volume 109}
          {:title "Homily Concerning Lowliness of Mind.",
           :id "viii",
           :volume 109}
          {:title "Instructions to Catechumens.",
           :id "ix",
           :volume 109}
          {:title "Three Homilies Concerning the Power of Demons.",
           :id "x",
           :volume 109}
          {:title
           "Homily on the Passage (Matt. xxvi. 19), 'Father If It Be Possible Let This Cup Pass from Me,' Etc., and Against Marcionists and Manichæans.",
           :id "xi",
           :volume 109}
          {:title
           "Homily on the Paralytic Let Down Through the Roof: and Concerning the Equality of the Divine Father and the Son.",
           :id "xii",
           :volume 109}
          {:title
           "Homily to Those Who Had Not Attended the Assembly: and on the Apostolic Saying, 'If Thine Enemy Hunger, Feed Him, Etc. (Rom. xii. 20), and Concerning Resentment of Injuries.'",
           :id "xiii",
           :volume 109}
          {:title
           "Homily Against Publishing the Errors of the Brethren, and Uttering Imprecations upon Enemies.",
           :id "xiv",
           :volume 109}
          {:title "Two Homilies on Eutropius.",
           :id "xv",
           :volume 109}
          {:title
           "A Treatise to Prove that No One Can Harm the Man Who Does Not Injure Himself.",
           :id "xvi",
           :volume 109}
          {:title "Letters of St. Chrysostom to Olympias.",
           :id "xvii",
           :volume 109}
          {:title
           "Correspondence of St. Chrysostom with the Bishop of Rome.",
           :id "xviii",
           :volume 109}
          {:title "The Homilies on the Statues to the People of Antioch.",
           :id "xix",
           :volume 109}
          {:title "The Homilies of St. John Chrysostom.",
           :id "iii",
           :volume 110}
          {:title "A Commentary on the Acts of the Apostles",
           :id "vi",
           :volume 111}
          {:title
           "The Homilies of St. John Chrysostom on Paul's Epistle to the Romans",
           :id "vii",
           :volume 111}
          {:title "Homilies on First Corinthians.",
           :id "iv",
           :volume 112}
          {:title "Homilies on Second Corinthians.",
           :id "v",
           :volume 112}
          {:title
           "The Commentary and Homilies of St. John Chrysostom on Galatians and Ephesians.",
           :id "iii",
           :volume 113}
          {:title
           "The Homilies of St. John Chrysostom on Philippians, Colossians, and Thessalonians.",
           :id "iv",
           :volume 113}
          {:title
           "The Homilies of St. John Chrysostom on Timothy, Titus, and Philemon.",
           :id "v",
           :volume 113}
          {:title
           "The Homilies of St. John Chrysostom on the Gospel of St. John.",
           :id "iv",
           :volume 114}
          {:title
           "The Homilies of St. John Chrysostom on the Epistle to the Hebrews.",
           :id "v",
           :volume 114}]}
 {:author "Eusebius"
  :books [{:title "The Church History of Eusebius.",
           :id "iii",
           :volume 201}
          {:title
           "The Life of Constantine with Orations of Constantine and Eusebius.",
           :id "iv",
           :volume 201}]}
 {:author "Socrates Scholasticus"
  :books [{:title "The Ecclesiastical History of Socrates Scholasticus.",
           :id "ii",
           :volume 202}]}
 {:author "Sozomen"
  :books [{:title "The Ecclesiastical History of Sozomen.",
           :id "iii",
           :volume 202}]}
 {:author "Theodoret"
  :books [{:title
           "The Ecclesiastical History, Dialogues, and Letters of Theodoret.",
           :id "iv",
           :volume 203}]}
 {:author "Gennadius"
  :books [{:title "Lives of Illustrius Men"
           :volume 203
           :id "v.iv"}]}
 {:author "Jerome",
  :books [{:title "Lives of Illustrious Men.",
           :id "v.iii",
           :volume 203}]}
 {:author "Rufinus"
  :books [{:title
           "Preface to the Commentary on the Benedictions of the Twelve Patriarchs.",
           :id "vi.iii",
           :volume 203}
          {:title "Preface to Book II.", :id "vi.iv", :volume 203}
          {:title "Translation of Pamphilus' Defence of Origen.",
           :id "vi.v",
           :volume 203}
          {:title
           "Rufinus's Epilogue to Pamphilus the Martyr's Apology for Origen; otherwise The Book Concerning the Adulteration of the Works of Origen.",
           :id "vi.vi",
           :volume 203}
          {:title
           "Preface to the Translations of Origen's Books Περὶ ᾽Αρχῶν.",
           :id "vi.vii",
           :volume 203}
          {:title "Preface to Book III. of the Περὶ ᾽Αρχῶν.",
           :id "vi.viii",
           :volume 203}
          {:title "Rufinus' Apology in Defence of Himself.",
           :id "vi.ix",
           :volume 203}
          {:title
           "The Letter of Anastasius, Bishop of the Church of Rome to John Bishop of Jerusalem Concerning the Character of Rufinus.",
           :id "vi.x",
           :volume 203}
          {:title
           "The Apology of Rufinus. Addressed to Apronianus, in Reply to Jerome's Letter to Pammachius.",
           :id "vi.xi",
           :volume 203}
          {:title
           "Jerome's Apology for Himself Against the Books of Rufinus.",
           :id "vi.xii",
           :volume 203}
          {:title "A Commentary on the Apostles' Creed.",
           :id "vi.xiii",
           :volume 203}
          {:title
           "The Preface to the Books of Recognitions of St. Clement.",
           :id "vi.xiv",
           :volume 203}
          {:title "Preface to the Translation of the Sayings of Xystus.",
           :id "vi.xv",
           :volume 203}
          {:title
           "Preface to the Two Books of Ecclesiastical History, Added by Rufinus to His Translation of Eusebius.",
           :id "vi.xvi",
           :volume 203}
          {:title
           "Rufinus' Preface to the Translation of Origen's Commentary on Psalms 36, 37, and 38.",
           :id "vi.xvii",
           :volume 203}
          {:title
           "Rufinus' Preface to the Translation of Origen's Commentary on the Epistle to the Romans.",
           :id "vi.xviii",
           :volume 203}
          {:title
           "The Peroration of Rufinus Appended to His Translation of Origen's Commentary on the Epistle to the Romans.",
           :id "vi.xix",
           :volume 203}
          {:title "Preface to Origen's Homilies on Numbers.",
           :id "vi.xx",
           :volume 203}]}
 {:author "St. Athanasius",
  :books [{:title "Against the Heathen. (Contra Gentes.)",
           :id "vi",
           :volume 204}
          {:title "The Incarnation of the Word.",
           :id "vii",
           :volume 204},
          {:title "Deposition of Arius. (Depositio Arii.)",
           :id "viii",
           :volume 204}
          {:title "Letter of Eusebius. (Epistola Eusebii.)",
           :id "ix",
           :volume 204}
          {:title "Statement of Faith. (Expositio Fidei.)",
           :id "x",
           :volume 204}
          {:title "On Luke x. 22. (Illud Omnia, &c.)",
           :id "xi",
           :volume 204}
          {:title "Encyclical Letter. (Epistola Encyclica.)",
           :id "xii",
           :volume 204}
          {:title "Defence Against the Arians. (Apologia Contra Arianos.)",
           :id "xiii",
           :volume 204},
          {:title "Defence of the Nicene Definition. (De Decretis.)",
           :id "xiv",
           :volume 204}
          {:title "Defence of Dionysius. (De Sententia Dionysii.)",
           :id "xv",
           :volume 204}
          {:title "Life of Antony. (Vita Antoni.)",
           :id "xvi",
           :volume 204}
          {:title
           "Circular to Bishops of Egypt and Libya. (Ad Episcopos Ægypti Et Libyæ Epistola Encyclica.)",
           :id "xvii",
           :volume 204}
          {:title "Apology to the Emperor. (Apologia Ad Constantium.)",
           :id "xviii",
           :volume 204}
          {:title "Defence of His Flight. (Apologia de Fuga.)",
           :id "xix",
           :volume 204}
          {:title "Arian History. (Historia Arianorum ad Monachos.)",
           :id "xx",
           :volume 204}
          {:title "Against the Arians. (Orationes contra Arianos IV.)",
           :id "xxi",
           :volume 204}
          {:title "On the Councils of Ariminum and Seleucia. (De Synodis.)",
           :id "xxii",
           :volume 204}
          {:title
           "Synodal Letter to the People of Antioch. (Tomus ad Antiochenos.)",
           :id "xxiii",
           :volume 204}
          {:title
           "Synodal Letter to the Bishops of Africa. (Ad Afros Epistola Synodica.)",
           :id "xxiv",
           :volume 204}
          {:title
           "Letters of Athanasius with Two Ancient Chronicles of His Life.",
           :id "xxv",
           :volume 204}]}
 {:author "St. Gegory of Nyssa"
  :books [{:title "Against Eunomius.", :id "viii.i", :volume 205}
          {:title "Answer to Eunomius' Second Book.",
           :id "viii.ii",
           :volume 205}
          {:title "On the Holy Spirit.", :id "viii.iii", :volume 205}
          {:title
           "On the Holy Trinity, and of the Godhead of the Holy Spirit.",
           :id "viii.iv",
           :volume 205}
          {:title "On 'Not Three Gods.'", :id "viii.v", :volume 205}
          {:title "On the Faith.", :id "viii.vi", :volume 205}
          {:title "Ascetic and Moral Treatises.",
           :id "ix",
           :volume 205}
          {:title "On the Making of Man.", :id "x.ii", :volume 205}
          {:title " On the Soul and the Resurrection.",
           :id "x.iii",
           :volume 205}
          {:title "The Great Catechism.", :id "xi.ii", :volume 205}
          {:title "Funeral Oration on Meletius.", :id "xii.ii", :volume 205}
          {:title "On the Baptism of Christ.", :id "xii.iii", :volume 205}
          {:title "Letters.",
           :id "xiii",
           :volume 205}]}
 {:author "St. Jerome"
  :books [{:title "The Letters of St. Jerome.",
           :id "v",
           :volume 206}
          {:title "The Life of Paulus the First Hermit.",
           :id "vi.i",
           :volume 206}
          {:title "The Life of S. Hilarion.", :id "vi.ii", :volume 206}
          {:title "The Life of Malchus, the Captive Monk.",
           :id "vi.iii",
           :volume 206}
          {:title "The Dialogue Against the Luciferians.",
           :id "vi.iv",
           :volume 206}
          {:title "The Perpetual Virginity of Blessed Mary.",
           :id "vi.v",
           :volume 206}
          {:title "Against Jovinianus.", :id "vi.vi", :volume 206}
          {:title "Against Vigilantius.", :id "vi.vii", :volume 206}
          {:title "To Pammachius against John of Jerusalem.",
           :id "vi.viii",
           :volume 206}
          {:title "Against the Pelagians.", :id "vi.ix", :volume 206}
          {:title "Prefaces to Jerome's Early Works.",
           :id "vii.ii",
           :volume 206}
          {:title
           "Prefaces to the Books of the Vulgate Version of the Old Testament.",
           :id "vii.iii",
           :volume 206}
          {:title "Prefaces to the Commentaries.",
           :id "vii.iv",
           :volume 206}]}
 {:author "St. Cyril"
  :books [{:title "The Catechetical Lectures",
           :id "ii",
           :volume 207}]}
 {:authnor "St. Gregory Nazianzen"
  :books [{:title "On Easter and His Reluctance.",
           :id "iii.iii",
           :volume 207}
          {:title
           "In Defence of His Flight to Pontus, and His Return, After His Ordination to the Priesthood, with an Exposition of the Character of the Priestly Office.",
           :id "iii.iv",
           :volume 207}
          {:title
           "To Those Who Had Invited Him, and Not Come to Receive Him.",
           :id "iii.v",
           :volume 207}
          {:title "Panegyric on His Brother S. Cæsarius.",
           :id "iii.vi",
           :volume 207}
          {:title "On his Sister Gorgonia.",
           :id "iii.vii",
           :volume 207}
          {:title
           "To His Father, When He Had Entrusted to Him the Care of the Church of Nazianzus.",
           :id "iii.viii",
           :volume 207}
          {:title
           "On His Father's Silence, Because of the Plague of Hail.",
           :id "iii.ix",
           :volume 207}
          {:title "On the Death of His Father.",
           :id "iii.x",
           :volume 207}
          {:title "On the Great Athanasius, Bishop of Alexandria.",
           :id "iii.xi",
           :volume 207}
          {:title "Introduction to the 'Theological' Orations.",
           :id "iii.xii",
           :volume 207}
          {:title
           "The First Theological Oration.  A Preliminary Discourse Against the Eunomians.",
           :id "iii.xiii",
           :volume 207}
          {:title "The Second Theological Oration.",
           :id "iii.xiv",
           :volume 207}
          {:title "The Third Theological Oration.  On the Son.",
           :id "iii.xv",
           :volume 207}
          {:title
           "The Fourth Theological Oration, Which is the Second Concerning the Son.",
           :id "iii.xvi",
           :volume 207}
          {:title "The Fifth Theological Oration. On the Holy Spirit.",
           :id "iii.xvii",
           :volume 207}
          {:title "Against The Arians, and Concerning Himself.",
           :id "iii.xviii",
           :volume 207}
          {:title "On the Arrival of the Egyptians.",
           :id "iii.xix",
           :volume 207}
          {:title
           "On the Words of the Gospel, 'When Jesus Had Finished These Sayings,' Etc.--S. Matt. xix. 1.",
           :id "iii.xx",
           :volume 207}
          {:title "On the Theophany, or Birthday of Christ.",
           :id "iii.xxi",
           :volume 207}
          {:title "Oration on the Holy Lights.",
           :id "iii.xxii",
           :volume 207}
          {:title "The Oration on Holy Baptism.",
           :id "iii.xxiii",
           :volume 207}
          {:title "On Pentecost.", :id "iii.xxiv",
           :volume 207}
          {:title
           "The Last Farewell in the Presence of the One Hundred and Fifty Bishops.",
           :id "iii.xxv",
           :volume 207}
          {:title
           "Funeral Oration on the Great S. Basil, Bishop of Cæsarea in Cappadocia.",
           :id "iii.xxvi",
           :volume 207}
          {:title "The Second Oration on Easter.",
           :id "iii.xxvii",
           :volume 207}
          {:title "Letters on the Apollinarian Controversy.",
           :id "iv.ii",
           :volume 207}
          {:title
           "Correspondence with Saint Basil the Great, Archbishop of Cæsarea.",
           :id "iv.iii",
           :volume 207}
          {:title "Miscellaneous Letters.",
           :id "iv.iv",
           :volume 207}]}
 {:author "St. Basil"
  :books [{:title "De Spiritu Sancto.",
           :id "vii",
           :volume 208}
          {:title "The Hexæmeron.",
           :id "viii",
           :volume 208}
          {:title "The Letters.",
           :id "ix",
           :volume 208}]}
 {:author "St. John of Damascus"
  :books [{:title "An Exact Exposition of the Orthodox Faith.",
           :id "iii.iv",
           :volume 209}]}
 {:author "St. Ambrose"
  :books [{:title "On the Duties of the Clergy.", :id "iv.i", :volume 210}
          {:title "On the Holy Spirit.", :id "iv.ii", :volume 210}
          {:title "On the Decease of His Brother Satyrus.",
           :id "iv.iii",
           :volume 210}
          {:title "Exposition of the Christian Faith.",
           :id "iv.iv",
           :volume 210}
          {:title "On the Mysteries.", :id "iv.v", :volume 210}
          {:title "Concerning Repentance.", :id "iv.vi", :volume 210}
          {:title "Concerning Virgins.", :id "iv.vii", :volume 210}
          {:title "Concerning Widows.", :id "iv.viii", :volume 210}
          {:title "Memorial of Symmachus, the Prefect of the City.",
           :id "v.i",
           :volume 210}
          {:title "Epistle XVII: To Valentinian II.",
           :id "v.ii",
           :volume 210}
          {:title "The Memorial of Symmachus, Prefect of the City.",
           :id "v.iii",
           :volume 210}
          {:title "Epistle XVIII: To Valentinian, in Reply to Symmachus.",
           :id "v.iv",
           :volume 210}
          {:title "Epistle XX: To Marcellina as to the Arian Party.",
           :id "v.v",
           :volume 210}
          {:title
           "Letter XXI: To Valentinian II., Declining Challenge of Auxentius.",
           :id "v.vi",
           :volume 210}
          {:title
           "Sermon Against Auxentius on the Giving Up of the Basilicas.",
           :id "v.vii",
           :volume 210}
          {:title
           "Letter XXII: To Marcellina on Finding the Bodies of SS. Gervasius and Protasius.",
           :id "v.viii",
           :volume 210}
          {:title
           "Letter XL: To Theodosius as to the Burning of a Jewish Synagogue.",
           :id "v.ix",
           :volume 210}
          {:title "Letter XLI: To Marcellina on the Same.",
           :id "v.x",
           :volume 210}
          {:title
           "Letter LI: To Theodosius After the Massacre at Thessalonica.",
           :id "v.xi",
           :volume 210}
          {:title "Letter LVII: To Eugenius, Reproving Him.",
           :id "v.xii",
           :volume 210}
          {:title
           "Letter LXI: To Theodosius, After His Victory Over Eugenius.",
           :id "v.xiii",
           :volume 210}
          {:title "Letter LXII: To Theodosius, Urging Him To Be Merciful.",
           :id "v.xiv",
           :volume 210}
          {:title "Epistle LXIII: To the Church at Vercellæ.",
           :id "v.xv",
           :volume 210}]}
 {:author "Sulpitius Severus"
  :books [{:title "On the Life of St. Martin.", :id "ii.ii", :volume 211}
          {:title "The Letters of Sulpitius Severus.",
           :id "ii.iii",
           :volume 211}
          {:title "Dialogues of Sulpitius Severus.",
           :id "ii.iv",
           :volume 211}
          {:title "The Doubtful Letters of Sulpitius Severus.",
           :id "ii.v",
           :volume 211}
          {:title "The Sacred History Of Sulpitius Severus.",
           :id "ii.vi",
           :volume 211}]}
 {:author "St. Vincent of Lerins"
  :books [{:title
           "The Commonitory of Vincent of Lérins, For the Antiquity and Universality of the Catholic Faith Against the Profane Novelties of All Heresies.",
           :id "iii",
           :volume 211}]}
 {:author "St. John Cassian"
  :books [{:title
           "The Twelve Books on the Institutes of the Cœnobia, and the Remedies for the Eight Principal Faults.",
           :id "iv.iii",
           :volume 211}
          {:title
           "The Conferences of John Cassian. Part I. Containing Conferences I-X.",
           :id "iv.iv",
           :volume 211}
          {:title
           "The Conferences of John Cassian. Part II. Containing  Conferences XI-XVII.",
           :id "iv.v",
           :volume 211}
          {:title
           "The Conferences of John Cassian. Part III. Containing  Conferences XVIII.-XXIV.",
           :id "iv.vi",
           :volume 211}
          {:title
           "The Seven Books of John Cassian on the Incarnation of  the Lord, Against Nestorius.",
           :id "iv.vii",
           :volume 211}]}
 {:author "St. Leo the Great"
  :books [{:title "Letters.", :id "ii.iv", :volume 212}
          {:title "Sermons.", :id "ii.v", :volume 212}]}
 {:author "St. Gregory the Great"
  :books [{:title "The Book of Pastoral Rule.", :id "iii.iv", :volume 212}
          {:title "Register of the Epistles of St. Gregory the Great.",
           :id "iii.v",
           :volume 212}
          {:title "Selected Epistles of Gregory the Great.",
           :id "ii",
           :volume 213}]}
 {:author "St. Ephraim the Syrian"
  :books [{:title
           "Introductory Dissertation:  Ephraim the Syrian and Aphrahat the Persian Sage.",
           :id "iii.iii",
           :volume 214}
          {:title "Ephraim Syrus:  The Nisibene Hymns.",
           :id "iii.iv",
           :volume 214}
          {:title
           "Ephraim Syrus:  Nineteen Hymns on the Nativity of Christ in the Flesh.",
           :id "iii.v",
           :volume 214}
          {:title
           "Ephraim Syrus:  Fifteen Hymns For the Feast of the Epiphany.",
           :id "iii.vi",
           :volume 214}
          {:title "Ephraim Syrus:  The Pearl.  Seven Hymns on the Faith.",
           :id "iii.vii",
           :volume 214}
          {:title "Ephraim Syrus:  Three Homilies.",
           :id "iii.viii",
           :volume 214}
          {:title "Aphrahat:  Select Demonstrations.",
           :id "iii.ix",
           :volume 214}]}])
 ;; {:author "Ecumenical Councils"
 ;;  :books [{:title
 ;;           "The First Ecumenical Council:  The First Council of Nice.",
 ;;           :id "vii",
 ;;           :volume 215}
 ;;          {:title
 ;;           "The Canons of the Councils of Ancyra, Gangra, Neocæsarea, Antioch and Laodicea, which Canons were Accepted and Received by the Ecumenical Synods.",
 ;;           :id "viii",
 ;;           :volume 215}
 ;;          {:title
 ;;           "The Second Ecumenical Council:  The First Council of Constantinople.",
 ;;           :id "ix",
 ;;           :volume 215}
 ;;          {:title "The Third Ecumenical Council:  The Council of Ephesus.",
 ;;           :id "x",
 ;;           :volume 215}
 ;;          {:title
 ;;           "The Fourth Ecumenical Council.  The Council of Chalcedon.",
 ;;           :id "xi",
 ;;           :volume 215}
 ;;          {:title
 ;;           "The Fifth Ecumenical Council.  The Second Council of Constantinople.",
 ;;           :id "xii",
 ;;           :volume 215}
 ;;          {:title
 ;;           "The Sixth Ecumenical Council.  The Third Council of Constantinople.",
 ;;           :id "xiii",
 ;;           :volume 215}
 ;;          {:title
 ;;           "The Canons of the Council in Trullo; Often Called The Quinisext Council.",
 ;;           :id "xiv",
 ;;           :volume 215}
 ;;          {:title
 ;;           "The Canons of the Synods of Sardica, Carthage, Constantinople, and Carthage Under St. Cyprian, Which Canons Were Received by the Council in Trullo and Ratified by II. Nice.",
 ;;           :id "xv",
 ;;           :volume 215}
 ;;          {:title
 ;;           "The Seventh Ecumenical Council.  The Second Council of Nice.",
 ;;           :id "xvi",
 ;;           :volume 215}
 ;;          {:title
 ;;           "Appendix containing Canons and Rulings not having Conciliar Origin but Approved by Name in Canon II. of the Synod in Trullo.",
 ;;           :id "xvii",
 ;;           :volume 215}]}])

;(parse-anpn-contents anti-nicene-contents)
;(parse-anpn-contents post-nicene-contents)
