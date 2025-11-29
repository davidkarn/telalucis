(ns frontend.ui
    (:require [frontend.bible :as bible]))

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
                      :on   {:click [:open-book {:author    (:author author)
                                                 :author-id (:author-id author)
                                                 :volume    (or (:volume book) (:volume author))
                                                 :id        (:id book)
                                                 :title     (:title book)}]}}
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

(defn lookup-bible-refs
  [book chapter store]
  (get-in store [:bible-refs (str book "-" chapter)]))

(defn get-verse-refs
  [refs verse]
  (flatten
   (map (fn [ref] (:refs ref))
        (filter (fn [ref] (= (:verse ref) (str (:num verse))))
                refs))))

(defn render-note-contents
  [contents]
  (cond (:content contents)
        (map render-note-contents (:content contents))
             
        (array? contents)
        (map render-note-contents contents)

        (string? contents)
        contents))

(defn render-doc-cell
  ([cell] (render-doc-cell cell false []))
  ([cell in-p notes]
   (cond (= (:node-type cell) "section")
         [:div.doc-section
          [:h3 (:title cell)]
          (map #(render-doc-cell % in-p notes) (:children cell))]

         (= "note" (:tag cell))
         [:span.note-ref {:id (str "ref-" (:id cell))} 
          [:a {:href (str "#" (:id cell))}
           (:n (:attrs (first (filter (fn [n] (= (:id (:attrs n)) (:id cell)))
                                      notes))))]]

         (and (:contents cell) (= "i" (:tag cell)))
         [:i (map #(render-doc-cell % in-p notes) (:contents cell))]
         
         (and (:contents cell) (= "b" (:tag cell)))
         [:b (map #(render-doc-cell % in-p notes) (:contents cell))]
         
         (:contents cell)
         (if in-p
           (map #(render-doc-cell % in-p notes) (:contents cell))
           [:div.doc-para
            [:p (map #(render-doc-cell % true (:notes cell)) (:contents cell))]
            (and (> (count (:notes cell)) 0)
                 [:div.doc-notes
                  (map (fn [note]
                         [:div.note {:id (:id (:attrs note))}
                          [:a.note-num {:href (str "#ref-" (:id (:attrs note)))}
                           (:n (:attrs note))]
                          [:div.note-contents (render-note-contents note)]])
                       (:notes cell))])])

         (vector? cell)
         (map #(render-doc-cell % in-p notes) cell)
         
         (string? cell)
         cell)))        

(defn render-doc
  [definition contents]
  [:div.document
   [:div.doc-header
    [:h3.author (:author definition)]
    [:h2.book-title (:title definition)]]
   [:div.doc-contents
    (render-doc-cell contents)]])

(defn render-bible-chapter
  [definition contents refs]
  (prn refs)
  [:div.document
   [:div.doc-header
    [:h3.author (bible/book-name (:book definition))]
    [:h2.book-title "Chapter " (:chapter definition)]]
   [:div.doc-contents
    (map
     (fn [verse]
       (let [verse-refs (get-verse-refs refs verse)]
         (prn [verse verse-refs])
         [:div.bible-verse-wrapper
          [:div.bible-verse
           [:div.verse-number (- (:num verse) 1)]
           [:div.verse-contents (:verse verse)]]
          [:div.verse-refs
           (map (fn [ref]
                  [:div.verse-ref
                   [:a.verse-link
                    {:href "javascript:false"
                     :on   {:click [:open-book {:author (:author ref)
                                                :id     (second (clojure.string/split
                                                                 (:book-id ref)
                                                                 ":"))
                                                :anchor (str "ref-" (:fn-id ref))
                                                :title  (:book ref)}]}}
                    [:span.ref-author (:author ref)]
                    " "
                    [:span.ref-book (:book ref)]]
                   " "
                   [:span.ref-snippet
                    (subs (clojure.string/join " " (filter string? (:para ref)))
                          0
                          120)]])
                verse-refs)]]))
     (drop 1 (:verses contents)))]])

(defn render-app
  [store]
  (prn (:bible-refs store))
  [:div.app
   (toc (:toc store))
   [:div.content
    (map
     (fn [doc]
       (cond (= (:type doc) :book)
             (render-doc doc (lookup-doc doc store))

             (= (:type doc) :bible)
             (render-bible-chapter doc
                                   (lookup-bible-chapter (:book doc)
                                                         (:chapter doc)
                                                         store)
                                   (lookup-bible-refs (:book doc) (:chapter doc) store))))
     (:open-docs store))]])

