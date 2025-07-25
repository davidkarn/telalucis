(ns telalucis.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [taoensso.nippy :as nippy])
  (:require [clojure.xml :as xml])
  (:require [clojure.data.json :as json])
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
  (if (= (:tag note) "scripRef")
    note
    (if (:content note)
      (first (filter identity (map note-has-scripture-ref (:content note))))
      nil)))
                                  

(defn chapter-notes
  [chapter-data author book chapter-name]
  (filter
   identity
   (flatten 
   (map (fn [para]
          (let [notes-with-ref (filter note-has-scripture-ref (:notes para))]
            (map (fn [root-ref]
                   (let [scrip-ref (note-has-scripture-ref root-ref)]
                     (if scrip-ref
                       {:ref     scrip-ref
                        :fn-id   (:id (:attrs root-ref))
                        :author  author
                        :book    book
                        :chapter chapter-name
                        :para    (flatten
                                  (filter #(not (and (= (:tag %) "note")
                                                     (not (= (:id %) (:id (:attrs root-ref))))))
                                          (:contents para)))}
                       nil)))
                 notes-with-ref)))
        (flatten (map :children (:children chapter-data)))))))


(defn translate-book
  [book]
  (case book
    "Ps"     "psalms"
    "1Cor"   "1-corinthi"
    "Ezra"   "1-esdras"
    "1John"  "1-john"
    "1Sam"   "1-kings--a"
    "1Macc"  "1-machabee"
    "1Chr"   "1-paralipo"
    "1Pet"   "1-peter"
    "1Thess" "1-thessalo"
    "1Tim"   "1-timothy"
    "2Cor"   "2-corinthi"
    "Neh"    "2-esdras--"
    "2John"  "2-john"
    "2Sam"   "2-kings--a"
    "2Macc"  "2-machabee"
    "2Chr"   "2-paralipo"
    "2Pet"   "2-peter"
    "2Thess" "2-thessalo"
    "2Tim"   "2-timothy"
    "3John"  "3-john"
    "1Kgs"   "3-kings"
    "2Kgs"   "4-kings"
    "Obad"   "abdias"
    "Acts"   "acts"
    "Sir"    "sirach"
    "Hag"    "aggeus"
    "Amos"   "amos"
    "Rev"    "apocalypse"
    "Bar"    "baruch"
    "Song"   "canticle-o"
    "Col"    "colossians"
    "Dan"    "daniel"
    "Deut"   "deuteronom"
    "Eccl"   "ecclesiast"
    "Eph"    "ephesians"
    "Esth"   "esther"
    "Exod"   "exodus"
    "Ezek"   "ezechiel"
    "Gal"    "galatians"
    "Gen"    "genesis"
    "Hab"    "habacuc"
    "Heb"    "hebrews"
    "Isa"    "isaias"
    "Jas"    "james"
    "Jer"    "jeremias"
    "Job"    "job"
    "Joel"   "joel"
    "John"   "john"
    "Jonah"  "jonas"
    "Josh"   "josue"
    "Jude"   "jude"
    "Judg"   "judges"
    "Jdt"    "judith"
    "Lam"    "lamentatio"
    "Lev"    "leviticus"
    "Luke"   "luke"
    "Mal"    "malachias"
    "Mark"   "mark"
    "Matt"   "matthew"
    "Mic"    "micheas"
    "Nah"    "nahum"
    "Num"    "numbers"
    "Hos"    "osee"
    "Phlm"   "philemon"
    "Phil"   "philippian"
    "PrAzar" "prayer-of-"
    "Prov"   "proverbs"
    "Rom"    "romans"
    "Ruth"   "ruth"
    "Zeph"   "sophonias"
    "Titus"  "titus"
    "Tob"    "tobias"
    "Wis"    "wisdom"
    "Zach"   "zacharias"))

(defn path-for-scripture-ref
  [script-path]
  (str app-root "public/data/bible/refs/"
       (:book script-path)
       "-"
       (:chapter script-path)
       ".json"))
       
(defn scripture-ref-to-path
  [ref]
  (let [[_ book chapter verse] (str/split (:parsed (:attrs (:ref ref))) #"\|")]
    {:book    (translate-book book)
     :chapter chapter
     :verse   verse}))

(defn compile-refs
  [refs]
  (reduce (fn [table ref]
            (let [scrip-path (scripture-ref-to-path ref)
                  key        (str/join ":" [(:book scrip-path)
                                            (:chapter scrip-path)])]
              (assoc table
                     key
                     (if (get key table)
                       (assoc (get key table)
                              :refs
                              (concat ref (:refs (get key table))))
                       (assoc scrip-path :refs [ref])))))
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
                    (pprint ["error" chapter])
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
  (loop [i   1
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
(save-books-to-disk (get-pages (parse-theology cf-2)) "augustine")
(save-books-to-disk (get-pages (parse-theology cf-3)) "augustine")
(save-books-to-disk (get-pages (parse-theology cf-4)) "augustine")
(save-books-to-disk (get-pages (parse-theology cf-5)) "augustine")

(get-pages contents)
(save-bible-to-disk (parse-bible (parse-theology douay)) "douay")
(save-books-to-disk (get-pages contents) "augustine")
(def chapt (read-chapter-from-disk "augustine" "the-confes-vi" "he-continu-vi-xii"))
(chapter-notes chapt "augustine" "confess" "confess-vi")
(write-refs-to-disk (chapter-notes chapt "augustine" "confess" "confess-vi"))

(def anti-nicene-contents
  [{:author "CLEMENT OF ROME",
    :id "ii",
    :volumne 1
    :books [{:title "First Epistle to the Corinthians",
             :id "ii.ii"}]}
   {:author "MATHETES",
    :id "iii",
    :volume 1
    :books [{:title "Epistle to Diognetus", :id "iii.ii"}]}
   {:author "POLYCARP",
    :id "iv",
    :volume 1
    :books [{:title "Epistle to the Philippians", :id "iv.ii"}
            {:title "The Martyrdom of Polycarp", :id "iv.iv"}]}
   {:author "IGNATIUS",
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
   {:author "BARNABAS",
     :id "vi",
     :volume 1
     :books [{:title "The Epistle of Barnabas", :id "vi.ii", :type nil}]}
   {:author "PAPIAS",
     :id "vii",
     :volume 1
     :books [{:title "Fragments", :id "vii.ii", :type nil}]}
   {:author "JUSTIN MARTYR",
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
   {:author "IRENÆUS",
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
   {:author "THE PASTOR OF HERMAS",
    :id "ii",
    :volume 2
    :books [{:title "Book First.—Visions", :id "ii.ii", :type nil}
            {:title "Book Second.—Commandments", :id "ii.iii", :type nil}
            {:title "Book Third.—Similitudes", :id "ii.iv", :type nil}
            {:title "Elucidations", :id "ii.v", :type nil}]}
   {:author "TATIAN",
    :id "iii",
    :volume 2
    :books [{:title "Address to the Greeks", :id "iii.ii", :type nil}
            {:title "Fragments", :id "iii.iii", :type nil}]}
   {:author "THEOPHILUS",
     :id "iv",
    :volume 2
    :books [{:title "Theophilus to Autolycus", :id "iv.ii", :type nil}]}
   {:author "ATHENAGORAS",
     :id "v",
    :volume 2
    :books [{:title "A Plea for the Christians", :id "v.ii", :type nil}
            {:title "The Resurrection of the Dead", :id "v.iii", :type nil}]}
   {:author "CLEMENT OF ALEXANDRIA",
    :id "vi",
    :volume 2
    :books [{:title "Exhortation to the Heathen", :id "vi.ii", :type nil}
            {:title "The Instructor", :id "vi.iii", :type nil}
            {:title "The Stromata, or Miscellanies", :id "vi.iv", :type nil}
            {:title "Who is the Rich Man that shall be saved?",
             :id "vi.v",
             :type nil})}
   {:author "Tertullian"    
    :books [{:title "Apology.", :id "iv.iii", :type nil :volume 3}
            {:title "On Idolatry.", :id "iv.iv", :type nil :volume 3}
            {:title "The Shows, or De Spectaculis.", :id "iv.v", :type nil :volume 3}
            {:title "The Chaplet, or De Corona.", :id "iv.vi", :type nil :volume 3}
            {:title "To Scapula.", :id "iv.vii", :type nil :volume 3}
            {:title "Ad Nationes.", :id "iv.viii", :type nil :volume 3}
            {:title "An Answer to the Jews.", :id "iv.ix", :type nil :volume 3}
            {:title "The Soul's Testimony.", :id "iv.x", :type nil :volume 3}
            {:title "A Treatise on the Soul.", :id "iv.xi", :type nil :volume 3}
            {:title "Anti-Marcion." :id "v" :volume 3}
            {:title "On Repentance.", :id "vi.ii", :type nil :volume 3}
            {:title "On Baptism.", :id "vi.iii", :type nil :volume 3}
            {:title "On Prayer.", :id "vi.iv", :type nil :volume 3}
            {:title "Ad Martyras.", :id "vi.v", :type nil :volume 3}
            {:title
             "The Passion of the Holy Martyrs Perpetua and Felicitas.",
             :id "vi.vi",
             :type nil :volume 3}
            {:title "On Patience.", :id "vi.vii", :type nil :volume 3}
            {:title "On the Pallium.", :id "iii.ii", :type nil :volume 4}
            {:title "On the Apparel of Women.", :id "iii.iii", :type nil :volume 4}
            {:title "On the Veiling of Virgins.", :id "iii.iv", :type nil :volume 4}
            {:title "To His Wife.", :id "iii.v", :type nil :volume 4}
            {:title "On Exhortation to Chastity.", :id "iii.vi", :type nil :volume 4}
            {:title "On Monogamy.", :id "iii.vii", :type nil :volume 4}
            {:title "On Modesty.", :id "iii.viii", :type nil :volume 4}
            {:title "On Fasting.", :id "iii.ix", :type nil :volume 4}
            {:title "De Fuga in Persecutione.", :id "iii.x", :type nil :volume 4}]}
   {:author "Minucius Felix.",
     :id "iv",
     :volume 4
     :books [{:title "The Octavius of Minucius Felix.",
              :id "iv.iii",
              :type nil}]}
   {:author "Commodianus.",
     :id "v",
     :volume 4
     :books [{:title "The Instructions of Commodianus.",
              :id "v.ii",
              :type nil}]}
   {:author "Origen.",
     :id "vi",
     :volume 4
     :books [{:title "Prologue of Rufinus.", :id "vi.iv", :type nil}
             {:title "Origen De Principiis.", :id "vi.v", :type nil}
             {:title
              "A Letter to Origen from Africanus About the History of Susanna.",
              :id "vi.vi",
              :type nil}
             {:title "A Letter from Origen to Africanus.",
              :id "vi.vii",
              :type nil}
             {:title "A Letter from Origen to Gregory.",
              :id "vi.viii",
              :type nil}
             {:title "Origen Against Celsus.", :id "vi.ix", :type nil}
             {:title "Origen's Commentary on the Gospel of John.",
              :id "xv.iii",
              :volume 9}
             {:title "Origen's Commentary on Matthew.",
              :id "xvi.ii",
              :volume 9}]}             
   {:author "Hippolytus.",
     :id "iii",
     :volume 5
     :books [{:title "The Refutation of All Heresies.",
              :id "iii.iii",
              :type nil}
             {:title "The Extant Works and Fragments of Hippolytus.",
              :id "iii.iv",
              :type nil}
             {:title
              "Appendix to the Works of Hippolytus. Containing Dubious and Spurious Pieces.",
              :id "iii.v",
              :type nil}]}
   {:author "Cyprian.",
     :id "iv",
     :volume 5
     :boooks [{:title
               "The Life and Passion of Cyprian, Bishop and Martyr. By Pontius the Deacon.",
               :id "iv.iii",
               :type nil}
              {:title "The Epistles of Cyprian.", :id "iv.iv", :type nil}
              {:title "The Treatises of Cyprian.", :id "iv.v", :type nil}
              {:title
               "The Seventh Council of Carthage under Cyprian. Concerning the Baptism of Heretics.",
               :id "iv.vi",
               :type nil}
              {:title
               "Treatises Attributed to Cyprian on Questionable Authority.",
               :id "iv.vii",
               :type nil}]}
   {:author "Caius.",
     :id "v",
     :volume 5
     :books [{:title "Fragments of Caius.", :id "v.iii", :type nil}]}
   {:author "Novatian.",
     :id "vi",
     :volume 5
     :books [{:title "A Treatise of Novatian Concerning the Trinity.",
              :id "vi.iii",
              :type nil}
             {:title "On the Jewish Meats.", :id "vi.iv", :type nil}]}
   {:author "Gregory Thaumaturgus.",
    :id "iii",
    :volume 6
    :books [{:title "Acknowledged Writings.", :id "iii.iii", :type "Part"}
            {:title "Dubious or Spurious Writings.",
             :id "iii.iv",
             :type "Part"}]}
   {:author "Dionysius.",
    :id "iv",
    :volume 6
    :books [{:title "Extant Fragments.", :id "iv.iii", :type nil}
            {:title "Exegetical Fragments.", :id "iv.iv", :type nil}]}
   {:author "Julius Africanus.",
    :id "v",
    :volume 6
    :books [{:title "The Epistle to Aristides.",
             :id "v.iii",
             :type "Section"}
            {:title
             "Narrative of Events Happening in Persia on the Birth of Christ.",
             :id "v.iv",
             :type "Section"}
            {:title
             "The Extant Fragments of the Five Books of the Chronography of Julius Africanus.",
             :id "v.v",
             :type "Section"}
            {:title "The Passion of St. Symphorosa and Her Seven Sons.",
             :id "v.vi",
             :type "Section"}
            {:title "Elucidations.", :id "v.vii", :type nil}]}
   {:author "Anatolius of Alexandria.",
    :id "vi.iii",
    :volume 6
    :books [{:title "The Paschal Canon of Anatolius of Alexandria.",
             :id "vi.iii.ii",
             :type nil}
            {:title "Fragments of the Books on Arithmetic.",
             :id "vi.iii.iii",
             :type nil}]}
   {:author "Alexander of Cappadocia.",
    :id "vi.iv",
    :volume 6
    :books [{:title "From the Epistles of Alexander.",
             :id "vi.iv.ii",
             :type nil}]}
   {:author "Theognostus of Alexandria.",
    :id "vi.v",
    :volume 6
    :books [{:title "From His Seven Books of Hypotyposes or Outlines.",
             :id "vi.v.ii",
             :type nil}]}
   {:author "Pierus of Alexandria.",
    :id "vi.vi",
    :volume 6
    :books [{:title
             "A Fragment of a Work of Pierius on the First Epistle of Paul to the Corinthians.",
             :id "vi.vi.ii",
             :type "Section"}
            {:title "A Section on the Writings of Pierius.",
             :id "vi.vi.iii",
             :type "Section"}]}
   {:author "Theonas of Alexandria.",
    :id "vi.vii",
    :volume 6
    :books [{:title
             "The Epistle of Theonas, Bishop of Alexandria, to Lucianus, the Chief Chamberlain.",
             :id "vi.vii.ii",
             :type nil}]}
   {:author "Phileas.",
    :id "vi.viii",
    :volume 6
    :books [{:title
             "Fragments of the Epistle of Phileas to the People of Thmuis.",
             :id "vi.viii.ii",
             :type nil}
            {:title
             "The Epistle of the Same Phileas of Thmuis to Meletius, Bishop of Lycopolis.",
             :id "vi.viii.iii",
             :type nil}]}
   {:author "Pamphilus.",
    :id "vi.ix",
    :volume 6
    :books [{:title
             "An Exposition of the Chapters of the Acts of the Apostles.",
             :id "vi.ix.ii",
             :type nil}]}
   {:author "Malchion.",
    :id "vi.x",
    :volume 6
    :books [{:title            
             "The Epistle Written by Malchion, In Name of the Synod of Antioch, Against Paul of Samosata."
             :id "vi.x.ii",
             :type "Section"}
            {:title
             "Fragments Apparently of the Same Epistle of the Synod of Antioch.",
             :id "vi.x.iii",
             :type "Section"}
            {:title
             "From the Acts of the Disputation Conducted by Malchion Against Paul of Samosata.",
             :id "vi.x.iv",
             :type "Section"}
            {:title "A Point in the Same Disputation.",
             :id "vi.x.v",
             :type "Section"}
            {:title "Elucidations.", :id "vi.x.vi", :type nil}]}
   {:author "Archelaus.",
     :id "vii",
     :volume 6
     :books [{:title "The Acts of the Disputation with the Heresiarch Manes.",
              :id "vii.iii",
              :type nil}
             {:title "A Fragment of the Same Disputation.",
              :id "vii.iv",
              :type nil}]}
   {:author "Alexander of Lycopolis.",
     :id "viii",
    :volume 6
    :books [{:title "Of the Manichæans.", :id "viii.iii", :type nil}
            {:title "Elucidation.", :id "viii.iv", :type nil}]}
   {:author "Peter of Alexandria.",
     :id "ix",
     :volume 6
     :books [{:title "The Genuine Acts of Peter.", :id "ix.iii", :type nil}
             {:title
              "The Canonical Epistle, with the Commentaries of Theodore Balsamon and John Zonaras.",
              :id "ix.iv",
              :type nil}
             {:title "Fragments from the Writings of Peter.",
              :id "ix.vi",
              :type nil}]}

   {:author "Alexander of Alexandria.",
     :id "x",
     :volume 6
     :books [{:title
              "Epistles on the Arian Heresy and the Deposition of Arius.",
              :id "x.iii",
              :type nil}]}
   {:author "Methodius.",
     :id "xi",
     :volume 6
     :books [{:title
              "The Banquet of the Ten Virgins; or Concerning Chastity.",
              :id "xi.iii",
              :type nil}
             {:title "Concerning Free-Will.", :id "xi.iv", :type nil}
      {:title "From the Discourse on the Resurrection.",
       :id "xi.v",
       :type nil}
      {:title "Fragments.", :id "xi.vi", :type nil}
      {:title
       "Oration Concerning Simeon and Anna On the Day that They Met in the Temple.",
       :id "xi.viii",
       :type nil}
      {:title "Oration on the Palms.", :id "xi.ix", :type nil}
      {:title
       "Three Fragments from the Homily on the Cross and Passion of Christ.",
       :id "xi.x",
       :type nil}
      {:title "Some Other Fragments of the Same Methodius.",
       :id "xi.xi",
       :type nil}
      {:title "Two Fragments, Uncertain.", :id "xi.xii", :type nil}]}
   {:author "Arnobius.",
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
