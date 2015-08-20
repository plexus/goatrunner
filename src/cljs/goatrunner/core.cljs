(ns goatrunner.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]))

(enable-console-print!)

(def block-px 36)

(defn bl->px [b]
  (* block-px b))

(def width-blocks 28)
(def height-blocks 21)

(def width-px (bl->px width-blocks))
(def height-px (bl->px height-blocks))



(def level [
[ 1 0 1 2 3 0 ]
[ 1 0 1 2 3 0 ]
])

(defonce app-state (atom {:text "Hello Chestnut!"}))

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
      (html [:svg {:width (str width-px "px") :height (str height-px "px") :style {:background-color "blue"}}
             (square 1 1 "red")
             (square 2 2 "green")]))))

(defn main []
  (om/root c-root
    app-state
    {:target (. js/document (getElementById "app"))}))
