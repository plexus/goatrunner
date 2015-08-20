(ns goatrunner.resources
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [hickory.core :as hickory]))

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

(defn svg-to-g [svg]
  (walk/postwalk (fn [node]
                   (if (= node :svg)
                     :g
                     node)) 
                 svg))

(defmacro svg-resource [name]
  (->
   (str name ".svg")
   io/resource
   io/file
   slurp
   hickory/parse
   hickory/as-hiccup
   extract-svg
   style-to-map
   svg-to-g))

