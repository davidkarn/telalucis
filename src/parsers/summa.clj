(ns parsers.summa
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.data.json :as json])
  (:require [hickory.core :as hickory])
  (:require [hickory.select :as sel])
  (:require [telalucis.scripture-refs :as scrip-ref])
  (:gen-class))

(defn index-of
  ([item coll]
    (index-of item coll 0))
  ([item coll from-idx]
    (loop [idx from-idx coll (seq (drop from-idx coll))]
      (if coll
        (if (= item (first coll))
          idx
          (recur (inc idx) (next coll)))
        -1))))

(defn pull-file
  [path]
  (hickory.core/as-hickory
   (hickory.core/parse
    (slurp (str "/Users/davidkarn/Projects/bookscontents/isidore.co/aquinas/english/summa/"
                path)))))

(defn get-book-questions
  [contents]
  (map (fn [question]
         (first
          (filter identity
                  (map (fn [p] (let [i (index-of question (:content p))]
                                 (if (> i -1)
                                   (let [a     (nth (:content p) i)
                                         label (nth (:content p) (+ i 1))]
                                     {:path  (str/replace (:href (:attrs a)) #"#.*" "")
                                      :num   (str/replace (nth (:content a) 0) "." "")
                                      :label (str/trim label)})
                                   nil)))
                       (sel/select (sel/tag :p) contents)))))
       (filter (fn [a] (and (:href (:attrs a))
                            (clojure.string/starts-with? (:href (:attrs a)) "FP/")))
               (sel/select (sel/tag :a) contents))))

(defn question-from-id
  [id]
  (Integer/parseInt (str/replace (str/replace id #".*Q" "") #"[^0-9].*" "")))

(defn article-from-id
  [id]
  (let [num (str/replace (str/replace id #".*A" "") #"[^0-9].*" "")]
    (if (> (count num) 0)
      (Integer/parseInt num)
      0)))

(defn book-from-id
  [id]
  (str/replace id #"Q.*" ""))

(defn format-text-content
  [el]
  (cond (string? el)
        el

        (= :i (:tag el))
        {:tag "i" :content (map format-text-content (:content el))}

        (= :b (:tag el))
        {:tag "b" :content (map format-text-content (:content el))}

        (= :p (:tag el))
        {:tag "span" :content (map format-text-content (:content el))}

        (= :a (:tag el))
        (if (re-matches #"http://drbo.org.*" (:href (:attrs el)))
          {:tag "scripRef"
           :attrs {:parsed (str "|" (str/replace
                                     (str/replace
                                      (str/replace
                                       (str (first (:content el))
                                            (if (re-matches #"^[^:]+$" (first (:content el)))
                                              ":1"
                                              ""))
                                       #": +"
                                       ":")
                                      #"(:| (?=\d+:\d+))" "|")
                                     #"( |\.)"
                                     ""))}
           :content (:content el)}
          (let [id (str/replace (:href (:attrs el)) #".*#" "")]
            {:tag "bookRef"
             :attrs {:author "Aquinas"
                     :book "Summa Theologica"
                     :id id}
             :content (:content el)}))

        true
        ""))

(defn format-question
  [contents]
  (loop [els             (filter (fn [el] (and (not (:align (:attrs el)))
                                               (not (#{:center :script :hr} (:tag el)))))
                                 (:content (first (sel/select (sel/tag :body) contents))))
         current-section {:contents []}
         all-sections    []]
    (let [el (first els)
          id (:id (:attrs el))]
      (cond (empty? els)
            (conj all-sections current-section)

            (and (= :a (:tag el))
                 id
                 (not (= "top" id)))
            (recur (drop 1 els)
                   (assoc (if (not (:id current-section))
                            current-section
                            {:contents []})
                          :id id
                          :question (question-from-id id)
                          :article (article-from-id id)
                          :book (book-from-id id))
                   (if (not (:id current-section))
                     all-sections
                     (conj all-sections current-section)))

            (not (:id current-section))
            (recur (drop 1 els)
                   current-section
                   all-sections)

            (= :h3 (:tag el))
            (recur (drop 1 els)
                   (assoc current-section
                          :title (str/join " " (filter string? (:content el)))
                          :contents (conj (:contents current-section)
                                         {:tag "b"
                                          :attrs {:heading "heading" :id (:id current-section)}
                                          :content (str/join " " (filter string? (:content el)))}))
                   all-sections)

            (= :p (:tag el))
            (recur (drop 1 els)
                   (assoc current-section
                          :contents (conj (:contents current-section)
                                         (format-text-content el)))
                   all-sections)

            (and (string? el) (> (count (str/trim el)) 0))
            (recur (drop 1 els)
                   (assoc current-section
                          :contents (conj (:contents current-section) (str/trim el)))
                   all-sections)

            true
            (recur (drop 1 els)
                   current-section
                   all-sections)))))

(defn format-summa
  []
  {:title "Summa Theologica"
   :node-type "section"
   :id "summa"
   :children (map
              (fn [book]
                (let [data (pull-file (str book ".html"))
                      questions (get-book-questions data)]
                  {:title (get {"FP" "Prima Pars"
                                "FS" "Prima Secundae Partis"
                                "SS" "Secunda Secundae Partis"
                                "TP" "Tertia Pars"
                                "XP" "Supplementum Tertiae Partis"} book)
                   :node-type "section"
                   :id (str "summa-" book)
                   :children (map
                              (fn [question]
                                (let [contents (pull-file (:path question))]
                                  {:title (str "Question " (:num question))
                                   :sub-title (:label question)
                                   :id (str "summa-" book "-" (:num question))
                                   :number (:num question)
                                   :children (format-question contents)}))
                              questions)}))
              ["FP" "FS" "SS" "TP" "XP"])})

(defn write-to-disk
  []
  (map
   (fn [book]
     (let [author "aquinas"
           id (:id book)]
       (.mkdir (java.io.File. (str telalucis.core/path "books/" author)))
       (.mkdir (java.io.File. (str telalucis.core/path "books/" author "/" id)))
       (with-open [wrtr (io/writer (str telalucis.core/path "books/" author "/" id "/" id ".json"))]
         (.write wrtr (json/write-str book)))))
   (:children (format-summa))))

(defn get-scrip-refs
  [section context book-id id question]
  (cond (string? section)
        nil

        (= "span" (:tag section))
        (filter identity
                (flatten (map (fn [s] (get-scrip-refs s context book-id id question))
                              (:content section))))

        (= "scripRef" (:tag section))
        {:ref section
         :author "St. Thomas Aquinas"
         :book "Summa Theologica"
         :chapter (:sub-title question)
         :book-id book-id
         :chapter-id id
         :para context}
        
        true
        nil))

(defn extract-refs
  [book]
  (flatten
   (map (fn [question]
          (flatten
           (map (fn [section]
                  (flatten
                   (map (fn [sec]
                          (get-scrip-refs sec
                                          sec
                                          (:id book)
                                          (:id section)
                                          question))
                        (:contents section))))
                (:children question))))
        (:children book))))
