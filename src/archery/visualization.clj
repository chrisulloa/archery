(ns archery.visualization
  (:require [archery.visitor :refer [shapes-collector]]
            [archery.shape :refer [shape rectangle-shape]]))

(defn shape->vega-data
  [[x1 y1 x2 y2]]
  {:x1 x1, :y1 y1, :x2 x2, :y2 y2})

(defn tree->vega-lite
  [tree]
  (let [{:keys [nodes rectangles points]} (shapes-collector (:root tree))]
    {:background "white",
     :config {:gridOpacity 0}
     :width 1000
     :height 1000
     :layer [{:data {:values (->> nodes
                                  (map shape)
                                  (mapv shape->vega-data))}
              :mark "rect"
              :encoding {:x {:field "x1", :type "quantitative"},
                         :x2 {:field "x2"},
                         :y {:field "y1", :type "quantitative"},
                         :y2 {:field "y2"},
                         :opacity {:value 0.5}
                         :stroke {:value "#023858"}}}
             {:data {:values (->> points
                                  (map rectangle-shape)
                                  (mapv shape->vega-data))}
              :mark "circle"
              :encoding {:color {:value "#3690c0"}
                         :x {:field "x1", :type "quantitative"},
                         :x2 {:field "x2"},
                         :y {:field "y1", :type "quantitative"},
                         :y2 {:field "y2"}}}
             {:data {:values (->> rectangles
                                  (map shape)
                                  (mapv shape->vega-data))}
              :mark "rect"
              :encoding {:x {:field "x1", :type "quantitative"},
                         :x2 {:field "x2"},
                         :y {:field "y1", :type "quantitative"},
                         :y2 {:field "y2"},
                         :stroke {:value "#74a9cf"}
                         :opacity {:value 0.8}}}]}))
