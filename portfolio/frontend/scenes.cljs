(ns frontend.scenes
  (:require [portfolio.replicant :refer-macros [defscene]]
            [portfolio.ui :as portfolio]
            [frontend.ui :as ui]))




(defscene empty-verse
  (ui/render-verse {}))

(defn main []
    (portfolio/start!
   {:config
    {:css-paths ["/styles.css"]
     :viewport/defaults
     {:background/background-color "#fdeddd"}}}))

