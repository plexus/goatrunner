(ns goatrunner.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [goatrunner.levels :refer [level-1]])
  (:require-macros [goatrunner.resources :refer [svg-resource]]))

(enable-console-print!)

(def block-px 36)

(defn bl->px [b]
  (* block-px b))

(def width-blocks 28)
(def height-blocks 21)

(def width-px (bl->px width-blocks))
(def height-px (bl->px height-blocks))

(def svg-goat (svg-resource "goat"))
(def svg-brick (svg-resource "brick"))
(def svg-ladder (svg-resource "ladder"))

(def tiles {:brick svg-brick
            :ladder svg-ladder})

(defn x-y [obj x y]
  (assoc obj 1 (merge (get obj 1) { :transform (str "translate(" x "," y ")")})))

(defn level->coords [level]
  (for [x (range width-blocks) 
        y (range height-blocks)
        :when (get-in level [y x])] [x y (get-in level [y x])]))

(defn filter-tiles 
  ([coords]
   (filter (fn [[_ _ tile]] (tile tiles)) coords))
  ([coords key]
   (filter (fn [[_ _ tile]] (= tile key)) coords)))

(defn svg-level-tiles [level]
  [:g
   (map (fn [[x y tile]] 
          (x-y (tiles tile) (bl->px x) (bl->px y))) 
        (-> level
            level->coords
            filter-tiles))])

(defn goat-pos [level]
  (let [[bx by _] (-> level-1
                      level->coords 
                      (filter-tiles :goat)
                      first)]
    [(bl->px bx) (bl->px by)]))

(defonce app-state (atom {:level level-1
                          :goat-x (first (goat-pos level-1))
                          :goat-y (last (goat-pos level-1))}))

(defn square [x y fill]
  [:rect {:x (bl->px x)
          :y (bl->px y)
          :width block-px
          :height block-px
          :style {:fill fill}}])



(defn c-root [app owner]
  (reify
    om/IRender
    (render [_]
      (html [:svg {:width (str width-px "px") :height (str height-px "px") :style {:background-color "black"}}
             (svg-level-tiles level-1)]))))

(defn main []
  (om/root c-root
    app-state
    {:target (. js/document (getElementById "app"))}))
