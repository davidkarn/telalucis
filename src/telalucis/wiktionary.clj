
(def xml-head "<?xml version='1.0' encoding='utf-8'?><mediawiki xmlns=\"http://www.mediawiki.org/xml/export-0.11/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.mediawiki.org/xml/export-0.11/ http://www.mediawiki.org/xml/export-0.11.xsd\" version=\"0.11\" xml:lang=\"en\">")
(def xml-foot "</mediawiki>")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn save-file
  [title data]
  (let [folder (str "/home/david/wiktionarydata/" (subs title 0 (min 3 (count title))) "/")
        file-path (str folder title)
        folder-file (io/file folder)]
    
    (if (not (.exists folder-file))
      (.mkdir folder-file))
    
    (println file-path)         
    
    (with-open [w (io/output-stream file-path)]
      (nippy/freeze-to-out! (java.io.DataOutputStream. w) data))))

(defn save-page
  [title latin-text]
  (if (not (re-matches #".*[^a-zA-Z].*" title ))
    (save-file title {:title title :text latin-text})))

;; this seems to hang after the file is completely processed. If it ever needs to
;; be read again, that should probably be fixed...
(defn split-wiktionary-chunks [file]
                                        
  (with-open [reader (clojure.java.io/reader file)]
    (loop [index 0
           has-started-page false
           current-title nil
           latin-content ""]
      (let [line  (.readLine reader)]
        (cond
          (> index 334447894)
          ""
          
          (and (not has-started-page) (= "  <page>" line))
          (do (recur (+ index 1) true nil ""))

          (and has-started-page (= current-title nil) (str/starts-with? line "    <title>"))
          (do (recur (+ index 1) true (nth (re-matches #" *<title>(.*)</title>" (str/trim line)) 1) ""))

          (and has-started-page
               (not (= current-title nil))
               (= latin-content "")
               (= "==Latin==" (str/trim line)))
          (do (recur (+ index 1) true current-title line))

          (and has-started-page
               (not (= latin-content ""))
               (or (re-matches #"^==[^=]+==$" (str/trim line))
                   (str/ends-with? (str/trim line) "</text>")))
          (do (save-page current-title latin-content)
              (recur (+ index 1) false nil ""))

          (and has-started-page (= "  </page>" line))
          (recur (+ index 1) false nil "")

          (and has-started-page (not (= latin-content "")))
          (recur (+ index 1) true current-title (str latin-content "\n" line))

          (and (not has-started-page) (= "  </page>" line))
          (recur (+ index 1) false nil "")

          true
          (recur (+ index 1)
                 has-started-page
                 current-title
                 latin-content))))))

(def wikifile "/home/david/Downloads/enwiktionary-20250301-pages-articles-multistream.xml")
(split-wiktionary-chunks wikifile)


(def parsed-wiki-path "/home/david/wiktionarydata")

(defn load-word
  [word]
  (with-open [rdr (io/input-stream (str parsed-wiki-path 
                                        "/" (subs word 0 (min 3 (count word)))
                                        "/" word))]
    (nippy/thaw-from-in! (java.io.DataInputStream. rdr))))

