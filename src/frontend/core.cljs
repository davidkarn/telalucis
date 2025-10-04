(ns frontend.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [replicant.dom :as r]
            [frontend.ui :as ui]))

(defn load-book
  [store book]
  (go (let [response (<! (http/get (str "/data/books/"
                                        (:author book) "/"
                                        (:id book) "/"
                                        (:id book) ".json")))]
        (prn (:body response))
        (swap! store assoc-in
               [:books (str (:author book) ":" (:id book))]
               (:body response)))))

(defn handle-event
  [store data]
  (case (first data)
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
                   :toc []})

    (load-toc store)))

(main)
