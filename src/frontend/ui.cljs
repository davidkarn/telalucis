(ns frontend.ui
    (:require [frontend.bible :as bible]))
  

;; app data structure
;;
;; {:open-docs 
;;   ({
;;     :type "book"
;;     :title ""
;;     :author ""
;;     :contents <content-block> | <section-block>[]
;;    } | {
;;     :type "bible-chapter"
;;     :translation ""
;;     :book ""
;;     :chapter <int>
;;     :id ""
;;     :verses {
;;       :num <int>
;;       :verse ""
;;     }
;;   })[]
;;  :available-docs {
;;   :<author-name> [
;;     {:title "" :id ""}
;;    ]
;;   }
;;  :toc {
;;    :author ""
;;    :books {
;;      :title ""
;;      :id ""
;;    }[]
;;  }[] 
;; }
;;
;;
;;
;; bible-book
;; {
;;     :type "bible"
;;     :translation ""
;;     :book ""
;;     :chapters {
;;       :id ""
;;       :title ""
;;       :verses {
;;         :num <int>
;;         :verse ""
;;       }[]
;;     }
;; }
;;
;; content-block
;; {
;;  :notes {
;;      :id ""
;;      :contents <content-span>[]
;;    }[]
;;  :contents content-span | content-block
;; }
;;
;; content-span
;; string | {
;;   :tag "p" | "i"
;;   :content <content-span>[]
;; } | footnote-reference
;;
;; footnote-reference
;; {
;;    :tag "note"
;;    :id  ""
;; }

(defn verse [{:keys [content]}]
  [:p
   content])

(defn toc
  [toc-data]
  [:div.toc
   [:div.author.bible
    [:strong.author-name "Douay"]
    [:div.author-books
     (map (fn [key]
            [:div.book
             [:strong.bible-book-name (bible/book-name key)]
             [:br]
             (map (fn [chapter]
                    [:a.bible-chapter {:href "javascript:false"
                                       :on   {:click [:open-bible-chapter key chapter]}}
                     chapter])
                  (range 1 (+ 1 (count (key bible/bible-chapters)))))])
          bible/ordered-books)]]
   (map (fn [author]
          [:div.author
           [:strong.author-name (:author author)]
           [:div.author-books
            (map (fn [book]
                   [:div.book
                    [:a.title
                     {:href "javascript:false"
                      :on   {:click [:open-book {:author (:author author)
                                                 :volume (or (:volume book) (:volume author))
                                                 :id     (:id book)
                                                 :title  (:title book)}]}}
                     (:title book)]])
                 (:books author))]])
        toc-data)])

(defn lookup-doc
  [doc store]
  (get (:books store)
       (str (:author doc) ":" (:id doc))))

(defn lookup-bible-chapter
  ([book chapter store]
   (lookup-bible-chapter "douay" book chapter store))
  ([translation book chapter store]
   (get-in store [:bible translation book :chapters (- chapter 1)] store)))

(defn render-doc-cell
  ([cell] (render-doc-cell cell false))
  ([cell in-p]
   (cond (= (:node-type cell) "section")
         [:div.doc-section
          [:h3 (:title cell)]
          (map #(render-doc-cell % in-p) (:children cell))]
         
         (and (:notes cell) (:contents cell))
         (if in-p
           (map render-doc-cell (:contents cell))
           [:p (map #(render-doc-cell % true) (:contents cell))])

         (vector? cell)
         (map #(render-doc-cell % in-p) cell)
         
         (string? cell)
         cell)))

(defn render-doc
  [definition contents]
  (prn definition contents)
  [:div.document
   [:div.doc-header
    [:h3.author (:author definition)]
    [:h2.book-title (:title definition)]]
   [:div.doc-contents
    (render-doc-cell contents)]])

(defn render-bible-chapter
  [definition contents]
  [:div.document
   [:div.doc-header
    [:h3.author (bible/book-name (:book definition))]
    [:h2.book-title "Chapter " (:chapter definition)]]
   [:div.doc-contents
    (map
     (fn [verse]
       [:div.bible-verse
        [:div.verse-number (- (:num verse) 1)]
        [:div.verse-contents (:verse verse)]])
     (drop 1 (:verses contents)))]])

(defn render-app
  [store]
  [:div.app
   (toc (:toc store))
   [:div.content
    (map
     (fn [doc]
       (cond (= (:type doc) :book)
             (render-doc doc (lookup-doc doc store))

             (= (:type doc) :bible)
             (render-bible-chapter doc (lookup-bible-chapter (:book doc) (:chapter doc) store))))
     (:open-docs store))]])

