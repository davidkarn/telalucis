(ns telalucis.dionysius)

(def contents (telalucis.core/parse-theology "/Users/davidkarn/Projects/bookscontents/dionysius.xml"))

(def dionysius-contents
  [{:author "Dionysius the Areopagite",
    :id "dionysius",
    :volume "dionysius"
    :books [{:title "On Divine Names",
             :id "i.ii",
             :type nil},
            {:title "Mystic Theology"
             :id "i.iii"}
            {:title "Letters"
             :id "i.iv"}
            {:title "Liturgy"
             :id "i.v"}
            {:title "On the Heavenly Hierarchy"
             :id "iii.ii"}
            {:title "Ecclesiastical Hierarchy"
             :id "iii.iii"}]}])
            

(defn parse-dionysius-works
  []
  (telalucis.core/parse-anpn-contents dionysius-contents))

(telalucis.core/save-tml-refs dionysius-contents)

