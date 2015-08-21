(ns goatrunner.core
  (:require [cljs.core.async :as async :refer [>! <! chan put! close! timeout]]
            [goatrunner.dimensions :refer [bl->px x-y width-blocks height-blocks block-px width-px height-px] :as dim]
            [goatrunner.key-queue :as keyq]
            [goatrunner.levels :refer [svg-level-tiles] :as l]
            [goatrunner.tiles :as tiles]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(enable-console-print!)

(def app-state (atom (l/level-state l/level-1)))

(def key-state (atom nil))

(def goat-speed 4)

(def dog-speed 2)

(def solid? #{:brick})
(def supportive? #{:brick :ladder})

(defn falling? [level x y]
  (let [floor-coords (dim/all-coords x (inc y))
        floor-tiles (l/select-tiles level floor-coords)]
    (not (some supportive? floor-tiles))))

(defn ladder? [level x y]
  (some #{:ladder} (l/select-tiles level (map dim/pos->coord [[x (+ 17 y)] [x (+ 18 y)]]))))

(defn can-move? [level dir x y]
  (not (some solid?
             (l/select-tiles level
              (dim/all-coords
               (dim/move dir x y goat-speed))))))


(defn attempt-move [level animal dir speed]
  (let [{x :x y :y} animal
        can-move (can-move? level dir x y)
        ladder (ladder? level x y)]
    (if (or (and (#{:left :right :down} dir) can-move)
            (and (#{:up :down} dir) ladder))
      (let [[new-x new-y] (dim/move dir x y speed)]
        (om/update! animal {:x new-x :y new-y})))))

(defn next-tick-goat [app]
  (let [{level :level goat-pos :goat-pos} app
        {x :x y :y} goat-pos
        key @key-state]
    (cond
      (falling? level x y) (attempt-move level goat-pos :down goat-speed)
      (#{:left :right :up :down} key) (attempt-move level goat-pos key goat-speed))))

(defn next-tick-dogs [level dogs goat]
  (doseq [idx (range (count dogs))]
    (let [dog (get dogs idx)
          {dx :x dy :y} dog
          {gx :x gy :y} goat
          ladder (ladder? level dx dy)
          left-ok (can-move? level :left dx dy)
          right-ok (can-move? level :left dx dy)
          dir (cond (falling? level dx dy) :down
                    (and (> gy dy) ladder) :down
                    (and (< gy dy) ladder) :up
                    (and (> gx dx) right-ok) :right
                    (and (< gx dx) left-ok) :left)]
      (if dir
        (attempt-move level dog dir dog-speed)))))

(defn next-tick [app]
  (when (om/transactable? app)
    (next-tick-goat app)
    (next-tick-dogs (:level app) (:dogs app) (:goat-pos app))))

(defn square [x y fill]
  [:rect {:x (bl->px x)
          :y (bl->px y)
          :width block-px
          :height block-px
          :style {:fill fill}}])

(defn keyboard-handler [data owner]
  (let [channel (:key-ch (om/get-shared owner))
        put-key (fn [key] (put! channel key))]
    (om/build keyq/KeyboardHandler {}
              {:opts {:keymap 
                      (atom {["right"] put-key
                             ["left"] put-key
                             ["up"] put-key
                             ["down"] put-key
                             ["keyup"] put-key
                             })}})))

(defn c-level-tiles [level owner]
  (reify
    om/IRender
    (render [this]
      (html [:g (svg-level-tiles level)]))))

(defn c-goat [goat-pos owner]
  (reify
    om/IRender
    (render [this]
      (html
       [:svg {:width 36
              :height 36
              :style {:left (:x goat-pos) :top (:y goat-pos) :position "absolute"}}
        tiles/svg-goat]))))

(defn c-dogs [dogs-pos owner]
  (reify
    om/IRender
    (render [this]
      (html 
       [:div
        (map (fn [dog]
               [:svg {:width 36 :height 36 :style {:position "absolute" :left (:x dog) :top (:y dog)}} 
                tiles/svg-dog])
             dogs-pos)]))))

(defn c-svg [data owner]
  (reify
    om/IRender
    (render [this]
      (html
       [:div
        [:svg {:width (str width-px "px")
               :height (str height-px "px")
               :style {:background-color "black" :left 0 :top 0 :position "absolute"}}
         (om/build c-level-tiles (:level data)) ]
        (om/build c-goat (:goat-pos data))
        ;;(om/build c-dogs (:dogs data))        
        ]))))

(defn c-debug [cursor owner]
  (reify
    om/IRender
    (render [this]
      (let [goat-pos (:goat-pos cursor)
            {x :x y :y} goat-pos
            level (:level cursor)]
        (html
         [:div {:style {:position "absolute" :top 1080 :left 0}}
          [:pre (str (into {} goat-pos))]
          [:pre (str "bx: " (dim/px->bl x) " by: " (dim/px->bl y))]
          [:pre (str "falling? " (falling? level x y))]
          [:pre (str "ladder? " (ladder? level x y))]
          [:pre (str {"@key-state:" @key-state})]])))))


(defn handle-key [key]
  (if (#{"left" "right" "up" "down"} key)
    (if (not (= (str @key-state) key))
      (swap! key-state (constantly (keyword key))))
    (if (= "keyup" key)
      (swap! key-state (constantly nil)))))

(defn c-root [app owner]
  (let [unmount-ch (chan)]
    (reify

      om/IDidMount
      (did-mount [this]
        (let [poll-ms 50
              key-ch (:key-ch (om/get-shared owner))]
          (go-loop [ticker (timeout poll-ms)]
            (alt!
              key-ch ([key _] 
                      (handle-key key)
                      (recur ticker))
              ticker ([_ _]
                      (next-tick (om/get-props owner))
                      (recur (timeout poll-ms)))
              unmount-ch ([_ _] 
                          ;; don't recur
                          )))))

      om/IWillUnmount
      (will-unmount [_]
        (put! unmount-ch true))

      om/IRender
      (render [this]
        (html 
         [:div {:class "wrapper"}
          (keyboard-handler app owner)
          (om/build c-svg app)
          (om/build c-debug app)
          ]
         )))))

(defn main []
  (om/root c-root
           app-state
           {:shared {:key-ch (chan 20)}
            :target (. js/document (getElementById "app"))}))
