(ns goatrunner.tiles
  (:require-macros [goatrunner.resources :refer [svg-resource]]))

(def svg-goat (svg-resource "goat-o"))
(def svg-brick (svg-resource "brick-o"))
(def svg-ladder (svg-resource "ladder-o"))
(def svg-dog (svg-resource "dog-o"))

(def tiles {:brick svg-brick
            :ladder svg-ladder
            :dog svg-dog})
