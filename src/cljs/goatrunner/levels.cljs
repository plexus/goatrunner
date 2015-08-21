(ns goatrunner.levels
  (:require [goatrunner.dimensions :refer [bl->px width-blocks height-blocks x-y] :as dim]
            [goatrunner.tiles :as t]))

(def _ false)
(def b :brick)
(def l :ladder)
(def G :goat)
(def D :dog)

(def level-1 
  [
   [ b _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ b  ]
   [ b _ _ l _ _ l _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ b  ]
   [ b G _ l _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ b  ]
   [ b b b b b b b b b l b b _ _ _ _ _ _ _ _ _ _ _ _ _ _ D b  ]
   [ b _ _ _ _ _ _ b _ l _ b b _ _ _ _ _ _ _ b b b l b b b b  ]
   [ b _ _ _ _ _ _ _ _ l _ _ b b _ _ _ _ _ _ _ _ _ l _ _ _ b  ]
   [ b _ _ _ _ _ _ _ _ _ _ _ _ b _ _ _ _ _ _ _ _ _ l _ _ _ b  ]
   [ b _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ l _ _ _ b  ]
   [ b _ _ D _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ D _ _ l _ _ _ b  ]
   [ b b b b b b b b b l _ _ _ b l b b b b b b b b b _ _ _ b  ]
   [ b _ _ _ _ _ _ _ _ l _ _ _ _ l _ _ _ _ _ _ _ _ _ _ _ _ b  ]
   [ b _ _ _ _ _ _ _ _ l _ _ _ _ l _ _ _ _ _ _ _ _ _ _ _ _ b  ]
   [ b _ _ _ _ _ _ _ _ l _ _ _ _ l _ _ _ _ _ _ _ _ _ _ _ D b  ]
   [ b _ _ _ _ _ _ _ _ l _ G _ _ b _ _ _ _ _ _ _ b b b b b b  ]
   [ b _ _ _ _ _ _ b b b b b b b b b l _ _ _ _ _ b _ _ _ _ b  ]
   [ b _ _ _ _ _ b _ _ _ _ _ _ _ _ _ l _ _ _ _ _ b _ _ _ _ b  ]
   [ b _ _ _ _ b _ _ _ _ _ _ _ _ _ _ l _ _ _ _ _ b _ _ _ _ b  ]
   [ b _ _ _ b _ _ _ _ _ _ _ _ _ _ _ l _ _ _ b b b b b b b b  ]
   [ b _ _ b _ _ _ _ _ _ _ _ _ _ _ _ l _ _ _ _ _ _ _ _ _ _ b  ]
   [ b _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ l _ _ _ _ _ _ _ _ _ _ b  ]
   [ b b b b b b b b b b b b b b b b b b b b b b b b b b b b  ]])

(defn level->coords [level]
  (for [x (range width-blocks) 
        y (range height-blocks)
        :when (get-in level [y x])] [x y (get-in level [y x])]))

(defn filter-tiles 
  ([coords]
   (filter (fn [[_ _ tile]] (tile t/tiles)) coords))
  ([coords key]
   (filter (fn [[_ _ tile]] (= tile key)) coords)))

(defn select-tile [level [x y]]
  (get-in level [y x]))

(defn select-tiles [level coords]
  (map (partial select-tile level) coords))

(defn transpose-coords [coords [xoffset yoffset]]
  (map (fn [[x y]] [(+ x xoffset) (+ y yoffset)])
       coords))

(defn select-tile-coords [level tile]
  (map dim/coord->pos
       (-> level-1
           level->coords 
           (filter-tiles tile))))

(defn coord->map [[x y]]
  {:x x :y y})

(defn level-state [level]
  (let [goat (first (select-tile-coords level :goat))
        dogs (select-tile-coords level :dog)]
    {:level level
     :goat-pos (coord->map goat)
     :dogs (mapv coord->map dogs)}))

(defn svg-level-tiles [level]
  (map (fn [[x y tile]] 
         (x-y (t/tiles tile) (bl->px x) (bl->px y))) 
       (-> level
           level->coords
           filter-tiles)))
