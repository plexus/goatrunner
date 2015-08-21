(ns goatrunner.core
  (:require [cljs.core.async :as async :refer [>! <! chan put! close! timeout]]
            [goatrunner.dimensions :refer [bl->px x-y width-blocks height-blocks block-px width-px height-px] :as dim]
            [goatrunner.key-queue :as keyq]
            [goatrunner.levels :refer [level-1 goat-pos svg-level-tiles] :as l]
            [goatrunner.tiles :as tiles]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(enable-console-print!)

(def app-state (atom {:level level-1
                      :goat-pos {:x (first (goat-pos level-1))
                                 :y (last (goat-pos level-1))}}))


(def key-state (atom nil))

(def speed 4)

(defn find-next-tile [level dir x y]
  (l/select-tile level (dim/next-tile dir x y)))

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
               (dim/move dir x y speed))))))


(defn attempt-move [app dir]
  (let [{x :x y :y} (:goat-pos app)
        level (:level app)
        can-move (can-move? level dir x y)
        ladder (ladder? level x y)]
    (println can-move)
    (if (or (and (#{:left :right :down} dir) can-move)
            (and (#{:up :down} dir) ladder))
      (let [[new-x new-y] (dim/move dir x y speed)]
        (om/update! app [:goat-pos] {:x new-x :y new-y})))))


(defn next-tick-goat [app]
  (let [{level :level {x :x y :y} :goat-pos} app
        key @key-state]
    (cond
      (falling? level x y) (attempt-move app :down)
      (#{:left :right :up :down} key) (attempt-move app key))))

(defn next-tick [app]
  (if (om/transactable? app)
    (next-tick-goat app)))

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
      (html (x-y tiles/svg-goat (:x goat-pos) (:y goat-pos))))))

(defn c-svg [data owner]
  (reify
    om/IRender
    (render [this]
      (html
       [:svg {:width (str width-px "px")
              :height (str height-px "px")
              :style {:background-color "black"}}
        (om/build c-level-tiles (:level data))
        (om/build c-goat (:goat-pos data))]))))

(defn c-debug [cursor owner]
  (reify
    om/IRender
    (render [this]
      (let [goat-pos (:goat-pos cursor)
            {x :x y :y} goat-pos
            level (:level cursor)]
        (html
         [:div
          [:pre (str (into {} goat-pos))]
          [:pre (str "bx: " (dim/px->bl x) " by: " (dim/px->bl y))]
          [:pre (str "falling? " (falling? level x y))]
          [:pre (str "ladder? " (ladder? level x y))]
          [:pre (str {"@key-state:" @key-state})]])))))


(defn handle-key [key app]
  (case key
    "left"  (swap! key-state (fn [_] :left))
    "right" (swap! key-state (fn [_] :right))
    "up"  (swap! key-state (fn [_] :up))
    "down" (swap! key-state (fn [_] :down))
    "keyup" (swap! key-state (fn [_] nil))
    nil))

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
                      (handle-key key (om/get-props owner))
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
