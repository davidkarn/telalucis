(ns frontend.ui)

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

(defn render-app
  [store]
  )

(defn render-verse [{:keys [content]}]
  [:p
   content])

