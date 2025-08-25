(ns frontend.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [replicant.dom :as r]
            [frontend/ui :as ui]))

(defn handle-event
  [store data]
  )

(defn load-toc [store]
  (go (let [response (<! (http/get "/toc.json"))]
        (prn  (:body response))
        (swap! store assoc :toc (:body response)))))

(defn main []
  (let [store (atom nil)
        el (js/document.getElementById "app")]

    (r/set-dispatch!
     (fn [_ event-handler-data]
       (handle-event store event-handler-data)))

    (add-watch store ::render
               (fn [_ _ _ data]
                 (ui/render-app data)
                 (r/render el)))

    (reset! store {:open-docs []
                   :toc []})))
