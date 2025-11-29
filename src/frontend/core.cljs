(ns frontend.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [replicant.dom :as r]
            [clojure.string :as string]            
            [devtools.core :as devtools]
            [frontend.bible :as bible]
            [frontend.ui :as ui]))

(devtools.core/install!)

(defn open-anchor!
  [anchor]
  (and anchor 
       (js/setTimeout
        (fn [] (set! js/window.location.href
                     (str (first (clojure.string/split js/window.location.href "#"))
                          "#"
                          anchor)))
        500)))

(defn load-book
  [store book]
  (go (let [response (<! (http/get (str "/data/books/"
                                        (or (:author-id book) (:author book)) "/"
                                        (:id book) "/"
                                        (:id book) ".json")))]
        (prn (:body response))
        (swap! store assoc-in
               [:books (str (:author book) ":" (:id book))]
               (:body response))
        (open-anchor! (:anchor book)))))

(defn load-bible-book
  ([store book] (load-bible-book store book "douay"))
  ([store book translation]
   (go (let [response (<! (http/get (str "/data/bible/" 
                                        translation "/"
                                        book ".json")))]
         (swap! store assoc-in
                [:bible translation book]
                (:body response))))))

(defn load-bible-refs
  [store book chapter]
  (go (let [key      (str book "-" chapter)
            response (<! (http/get (str "/data/bible/refs/" key ".json")))]
        (swap! store assoc-in
               [:bible-refs key]
               (:body response)))))

(defn handle-event
  [store data]
  (case (first data)
    :open-bible-chapter
    (let [book (bible/book-internal-name (second data))
          chapter (nth data 2)]
      (load-bible-book store book)
      (load-bible-refs store book chapter)
      (swap! store assoc :open-docs [{:type :bible
                                      :book book
                                      :chapter chapter}]))
    
    :open-book
    (do (load-book store (second data))
        (swap! store assoc :open-docs [(assoc (second data) :type :book)]))))

(defn load-toc [store]
  (go (let [response (<! (http/get "/data/toc.json"))]
        (prn (:body response))
        (swap! store assoc :toc (:body response)))))

(defn view
  [store]
  (ui/render-app store))

(defn main []
  (let [store (atom {})
        el js/document.body]    

    (r/set-dispatch!
     (fn [_ event-handler-data]
       (handle-event store event-handler-data)))

    (add-watch store ::render
               (fn [_ _ _ data]
                 (r/render el (ui/render-app data))))

    (reset! store {:open-docs []
                   :books {}
                   :bible {"douay" {}}
                   :toc []})

    (load-toc store)))

(main)
