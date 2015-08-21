(ns goatrunner.dimensions)

(def block-px 36)

(defn floor [f]
  (.floor js/Math f))

(defn ceil [f]
  (.ceil js/Math f))

(defn round [f]
  (.round js/Math f))

(defn bl->px [b]
  (* block-px b))

(defn px->bl [px]
  (/ px block-px))

(defn pos->coord [coord]
  (mapv (comp round px->bl) coord))

(defn coord->pos [pos]
  (mapv bl->px pos))

(def width-blocks 28)
(def height-blocks 21)

(def width-px (bl->px width-blocks))
(def height-px (bl->px height-blocks))

(defn x-y [obj x y]
  (assoc obj 1 (merge (get obj 1) {:transform (str "translate(" x "," y ")") 
                                   :key (str x "-" y)})))

(defn id [a & args] a)

(def directions {:left  [- id floor]
                 :right [+ id ceil]
                 :up    [id - floor]
                 :down  [id + ceil]
                 })

(defn move [dir x y offset]
  (let [[xop yop _] (dir directions)]
    [(xop x offset) (yop y offset)]))

(defn next-tile [dir x y]
  (let [[_ _ round] (dir directions)]
    (mapv (comp round px->bl) (move dir x y 1))))

(defn all-coords 
  ([[x y]] (all-coords x y))
  ([x y]
   (set (for [xop [floor ceil]
              yop [floor ceil]] [(xop (px->bl x)) (yop (px->bl y))]))))
