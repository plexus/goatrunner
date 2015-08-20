(ns goatrunner.resources
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [hickory.core :as hickory]
            [hickory.zip :refer :all]))

(defn extract-svg
  "Not the most elegant, traverse a Hiccup style parsing of an SVG to find the <svg>"
  [html]
  (first (filter #(and (vector? %) (= :svg (first %))) 
                 (tree-seq #(or (seq? %) (vector? %)) identity html))))

(defn style-str->map [styles]
  (into {}
        (->>
         (str/split styles #";")
         (map #(str/split % #":"))
         (map (fn [[k v]] [(keyword k) v])))))

(defn style-to-map [svg]
  (walk/postwalk (fn [node]
                   (if (and (map? node) (:style node))
                     (merge node {:style (style-str->map (:style node))})
                     node))
                 svg))

(defn svg-to-g [[_ _ & children]]
  (into [:g {}] children))


(defn print-tree [original]
  (loop [loc (zip/seq-zip (seq original))]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
                (do (println (zip/node loc))
                    loc))))))

(defn clean-up-svg [svg]
  (loop [loc (hiccup-zip svg)]
    (if (zip/end? loc)
      (zip/root loc)
      (let [[tag attrs] (zip/node loc)]
        (if (or (= tag :metadata) (and (keyword? tag) (re-find #":" (name tag))))
          (recur (zip/remove loc))
          (recur (zip/next loc)))))))

(defn remove-text-nodes [svg]
  (vec
   (remove string?
           (map (fn [e] 
                  (if (vector? e) 
                    (remove-text-nodes e)
                    e)) svg))))

(defn -svg-resource [name]
  (->
   (str name ".svg")
   io/resource
   io/file
   slurp
   hickory/parse
   hickory/as-hiccup
   extract-svg
   clean-up-svg
   remove-text-nodes
   style-to-map
   svg-to-g))

(defmacro svg-resource [name]
  (-svg-resource name))



