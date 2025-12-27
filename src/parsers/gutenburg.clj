(ns parsers.gutenburg
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.data.json :as json])
  (:require [hickory.core :as hickory])
  (:require [hickory.select :as sel])
  (:require [telalucis.scripture-refs :as scrip-ref])
  (:gen-class))

(def path "gloriesofmary.html")

(defn footnote-id-num
  [footnote-id]
  (if (not (string? footnote-id))
    nil
    (str/replace (str/replace footnote-id "Footnote_" "")
                 "FNanchor_" "")))

(defn pull-file
  [path]
  (hickory.core/as-hickory
   (hickory.core/parse
    (slurp (str "/Users/davidkarn/Projects/bookscontents/" path)))))

(defn get-body
  [contents]
  (:content (first (sel/select (sel/tag :body) contents))))

(defn get-header-text
  [header]
  (get-in (sel/select (sel/tag :h2) header)
          [0 :content 0]))

(defn new-chapter
  [i tag]
  {:title     (get-header-text tag)
   :note-type "section"
   :id        i
   :number    i
   :children  []})

(defn add-paragraph-to-chapter
  [chapter para]
  (assoc chapter
         :children
         (conj (:children chapter) para)))

(defn get-footnote-tags
  [tags]
  (filter (fn [tag] (:tag tag))
          (:content 
           (first
            (filter (fn [tag] (= "footnotes" (get-in tag [:attrs :class])))
                    tags)))))
             
(defn get-footnotes-table
  [tags]
  (reduce
   (fn [acc tag]
     (let [note (first (sel/select (sel/class "pginternal") tag))
           id   (footnote-id-num (get-in note [:attrs :id]))
           body (rest (:content (first (sel/select (sel/tag :p) tag))))
           scrip-ref (telalucis.scripture-refs/parse-ref (apply str body) true)]
       (assoc acc id
              {:tag     "note"
               :attrs   {:id id :n id}
               :content (if scrip-ref
                          (conj body
                                {:tag "scripRef"
                                 :attrs {:id id
                                         :parsed (telalucis.scripture-refs/to-parsed scrip-ref)
                                         :passage (telalucis.scripture-refs/print-ref scrip-ref)}
                                 :content (telalucis.scripture-refs/print-ref scrip-ref)})
                          body)})))
   {}
   tags))

(defn read-paragraph
  [paragraph footnotes-table]
  (loop [built-para   []
         notes        []
         contents     (rest (:content paragraph))
         current-item (first (:content paragraph))]
    (cond
      (nil? current-item)
      {:notes    notes
       :contents built-para}

      (string? current-item)
      (recur (conj built-para current-item)
             notes
             (rest contents)
             (first contents))

      (= "fnanchor pginternal" (get-in current-item [:attrs :class]))
      (let [note-id (footnote-id-num (get-in current-item [:attrs :id]))]
        (recur (conj built-para {:tag "note"
                                 :id  note-id})
               (conj notes (get footnotes-table note-id))
               (rest contents)
               (first contents)))

      (= :i (:tag current-item))
      (recur (conj built-para {:tag "i" :content (:content current-item)})
             notes
             (rest contents)
             (first contents))

      (= :b (:tag current-item))
      (recur (conj built-para {:tag "b" :content (:content current-item)})
             notes
             (rest contents)
             (first contents))

      true
      (recur built-para
             notes
             (rest contents)
             (first contents)))))

(defn read-book-contents [body]
  (let [footnotes-table (get-footnotes-table (get-footnote-tags body))]
    (loop [current-chapter nil
           book            []
           contents        (rest body)
           current-tag     (first body)
           i               0]
      (cond
        (nil? current-tag)
        (conj book current-chapter)

        (string? current-tag)
        (recur current-chapter
               book
               (rest contents)
               (first contents)
               (+ i 1))

        (= "chapter" (get-in current-tag [:attrs :class]))
        (if (= current-chapter nil)
          (recur (new-chapter i current-tag) book (rest contents) (first contents) (+ 1 i))
          (recur (new-chapter i current-tag)
                 (conj book current-chapter)
                 (rest contents)
                 (first contents)
                 (+ 1 i)))

        (= :p (:tag current-tag))
        (recur (add-paragraph-to-chapter current-chapter
                                         (read-paragraph current-tag footnotes-table))
               book
               (rest contents)
               (first contents)
               (+ 1 i))

        (#{:h1 :h2 :h3 :h4 :h5 :h6} (:tag current-tag))
        (recur (add-paragraph-to-chapter current-chapter
                                         {:notes    []
                                          :contents [{:tag     (:tag current-tag)
                                                      :content (:content current-tag)}]})
               book
               (rest contents)
               (first contents)
               (+ 1 i))

        true
        (recur current-chapter book (rest contents) (first contents) (+ 1 i))))))
             

(def books
  [{:title "The Glories of Mary"
    :author "St. Alphonsus Liguori"
    :id "glories-mary"
    :path "gloriesofmary.html"}
   {:title "Life of St. Malachy of Armagh"
    :author "St. Bernard of Clairvaux"
    :id "life-st-malachy"
    :path "bernard_malachy.html"}])
   
(defn write-to-disk
  [books]
  (map
   (fn [book]
     (let [author   (:author book)
           contents (read-book-contents (get-body (pull-file (:path book))))
           title    (:title book)
           id       (:id book)]
       (.mkdir (java.io.File. (str telalucis.core/path "books/" author)))
       (.mkdir (java.io.File. (str telalucis.core/path "books/" author "/" id)))
       (with-open [wrtr (io/writer (str telalucis.core/path "books/" author "/" id "/" id ".json"))]
         (.write wrtr (json/write-str {:title title
                                       :node-type "section"
                                       :id id
                                       :children contents})))))
   books))


(defn get-scrip-refs
  [book section context chapter-id chapter-title]  
  (cond (string? section)
        nil

        (= "note" (:tag section))
        (filter identity
                (flatten (map (fn [s] (get-scrip-refs book s context chapter-id chapter-title))
                              (:content section))))

        (= "scripRef" (:tag section))
        {:ref section
         :author (:author book)
         :book (:title book)
         :book-id (:id book)
         :chapter-id chapter-id
         :chapter chapter-title
         :para context}
        
        true
        nil))

(defn extract-refs
  [book]
  (let [contents (read-book-contents (get-body (pull-file (:path book))))]
    (filter identity
            (flatten
             (map
              (fn [chapter]
                (map (fn [section]
                       (map (fn [note]
                              (get-scrip-refs book
                                              note
                                              (:children chapter)
                                              (:id chapter)
                                              (:title chapter)))
                            (:notes section)))
                     (:children chapter)))
              contents)))))

(map write-to-disk books)
(map (fn [book] (telalucis.core/write-refs-to-disk (extract-refs book)))
     books)
