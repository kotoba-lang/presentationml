(ns presentationml.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [presentationml.core :as pml]))

(deftest renders-slide-and-presentation
  (is (str/includes? (pml/slide-xml (pml/slide "<p:sp/>")) "<p:sp/>"))
  (is (str/includes? (pml/presentation-xml 1 {:width 1280 :height 720}) "<p:sldId id=\"256\"")))

(deftest builds-package-map
  (let [pkg (pml/package-map {:slides [(pml/slide "<p:sp/>")]})]
    (is (contains? pkg "[Content_Types].xml"))
    (is (contains? pkg "ppt/slides/slide1.xml"))
    (is (str/includes? (get pkg "ppt/_rels/presentation.xml.rels") "slide1.xml"))))
