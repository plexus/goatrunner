(ns goatrunner.core
  (:require [goatrunner.levels :refer [level-1]]
            [goatrunner.key-queue :as keyq]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [cljs.core.async :as async :refer [>! <! chan put! close! timeout]])
  (:require-macros [goatrunner.resources :refer [svg-resource]]
                   [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(def block-px 36)

(defn bl->px [b]
  (* block-px b))

(def width-blocks 28)
(def height-blocks 21)

(def width-px (bl->px width-blocks))
(def height-px (bl->px height-blocks))

(def svg-goat (svg-resource "goat-o"))
(def svg-brick (svg-resource "brick-o"))
(def svg-ladder (svg-resource "ladder-o"))
(def svg-dog (svg-resource "dog-o"))

(def tiles {:brick svg-brick
            :ladder svg-ladder
            :dog svg-dog})


(defn x-y [obj x y]
  (try
    (assoc obj 1 (merge (get obj 1) { :transform (str "translate(" x "," y ")")}))
    (catch js/Object error
      (println error "while working on" obj))))

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
  (map (fn [[x y tile]] 
         (x-y (tiles tile) (bl->px x) (bl->px y))) 
       (-> level
           level->coords
           filter-tiles)))

(defn goat-pos [level]
  (let [[bx by _] (-> level-1
                      level->coords 
                      (filter-tiles :goat)
                      first)]
    [(bl->px bx) (bl->px by)]))



(def app-state (atom {:level level-1
                      :goat-x (first (goat-pos level-1))
                      :goat-y (last (goat-pos level-1))
                      :moving nil}))

(defn next-tick [state]
  (case (:moving @app-state)
    :right (swap! app-state #(update-in % [:goat-x] (partial + 4)))
    :left (swap! app-state #(update-in % [:goat-x] (fn [i] (- i 4))))
    true)
  state)

;; (defn next-tick []
;;   (case moving
;;     :up (update-in state :goat-y dec)
;;     :down (update-in state :goat-y inc)
;;     :left (update-in state :goat-x dec)
;;     :right (update-in state :goat-x inc)
;;     state))

(defn square [x y fill]
  [:rect {:x (bl->px x)
          :y (bl->px y)
          :width block-px
          :height block-px
          :style {:fill fill}}])

(defn keyboard-handler [data owner]
  (om/build keyq/KeyboardHandler {}
            {:opts {:keymap 
                    (atom {["right"] #(om/transact! data :moving (constantly :right))
                           ["left"]  #(om/transact! data :moving (constantly :left))
                           ["keyup"] #(om/transact! data :moving (constantly nil))})}}))

(defn c-root [app owner]
  (reify
    om/IDidMount
    (did-mount [this]

      (go-loop []
        (<! (timeout 50))
        (om/transact! app next-tick)
        (recur)))

    om/IRender
    (render [_]
      (html 
       [:div {:class "wrapper"}
        (keyboard-handler app owner)
        (into
         [:svg {:width (str width-px "px")
                :height (str height-px "px")
                :style {:background-color "black"}}
          (x-y svg-goat (:goat-x app) (:goat-y app))
          (x-y svg-goat (:goat-x app) (:goat-y app))
          ]
         (svg-level-tiles (:level app)))
        [:pre (str ":moving " (:moving app) " :goat-x " (:goat-x app))]]))))



;; (x-y svg-goat (:goat-x app) (:goat-y app))


(defn main []
  (om/root c-root
           app-state
           {:target (. js/document (getElementById "app"))}))
