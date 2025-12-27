(ns parsers.gutenburg
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.data.json :as json])
  (:require [hickory.core :as hickory])
  (:require [hickory.select :as sel])
  (:require [telalucis.scripture-refs :as scrip-ref])
  (:gen-class))

(def path "gloriesofmary.html")

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
  {:title (get-header-text tag)
   :note-type "section"
   :id i
   :number i
   :children []})

(defn add-paragraph-to-chapter
  [chapter para]
  (assoc chapter
         :children
         (conj (:children chapter) para)))

(defn read-paragraph
  [paragraph]
  (loop [built-para []
         contents (rest (:content paragraph))
         current-item (first (:content paragraph))]
    (cond
      (nil? current-item)
      {:notes []
       :contents built-para}
      
      (string? current-item)
      (recur (conj built-para current-item)
             (rest contents)
             (first contents))

      (= "fnanchor pginternal" (:get-in current-item [:attrs :class]))
      (recur (conj built-para {:tag "note" :id (get-in current-item [:attrs :id])})
             (rest contents)
             (first contents))

      (= :i (:tag current-item))
      (recur (conj built-para {:tag "i" :content (:content current-item)})
             (rest contents)
             (first contents))
      
      (= :b (:tag current-item))
      (recur (conj built-para {:tag "b" :content (:content current-item)})
             (rest contents)
             (first contents))

      true
      (recur built-para
             (rest contents)
             (first contents)))))

(defn read-book-contents [body]
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
      (recur (add-paragraph-to-chapter current-chapter (read-paragraph current-tag))
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
      (recur current-chapter book (rest contents) (first contents) (+ 1 i)))))
             
             
             
             
              
              
             
             
               
           
