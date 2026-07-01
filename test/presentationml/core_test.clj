(ns presentationml.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [presentationml.core :as pml]
            [presentationml.parse :as parse]))

(deftest renders-slide-and-presentation
  (is (str/includes? (pml/slide-xml (pml/slide "<p:sp/>")) "<p:sp/>"))
  (is (str/includes? (pml/presentation-xml 1 {:width 1280 :height 720}) "<p:sldId id=\"256\"")))

(deftest builds-package-map
  (let [pkg (pml/package-map {:slides [(pml/slide "<p:sp/>")]})]
    (is (contains? pkg "[Content_Types].xml"))
    (is (contains? pkg "ppt/slides/slide1.xml"))
    (is (str/includes? (get pkg "ppt/_rels/presentation.xml.rels") "slide1.xml"))
    (is (pml/valid-slide? (pml/slide "<p:sp/>")))
    (is (pml/valid-package-map? pkg))))

(deftest parses-package-fixture
  (let [entries {"docProps/core.xml" "<cp:coreProperties><dc:title>Fixture deck</dc:title></cp:coreProperties>"
                 "ppt/presentation.xml" "<p:presentation><p:sldSz cx=\"9144000\" cy=\"5143500\"/></p:presentation>"
                 "ppt/theme/theme1.xml" "<a:theme><a:clrScheme><a:accent1><a:srgbClr val=\"ABCDEF\"/></a:accent1></a:clrScheme></a:theme>"
                 "ppt/slides/slide1.xml"
                 (str "<p:sld><p:cSld><p:spTree>"
                      "<p:sp><p:nvSpPr><p:cNvPr id=\"2\" name=\"Title\"/></p:nvSpPr>"
                      "<p:spPr><a:xfrm><a:off x=\"914400\" y=\"914400\"/><a:ext cx=\"1828800\" cy=\"914400\"/></a:xfrm></p:spPr>"
                      "<p:txBody><a:p><a:r><a:rPr sz=\"2800\"><a:solidFill><a:srgbClr val=\"123456\"/></a:solidFill></a:rPr><a:t>Hello</a:t></a:r></a:p></p:txBody></p:sp>"
                      "</p:spTree></p:cSld></p:sld>")}
        deck (parse/deck entries)
        shape (-> deck :presentationml/slides first :presentationml/shapes first)]
    (is (parse/valid-deck? deck))
    (is (= "Fixture deck" (:presentationml/title deck)))
    (is (= 10.0 (:presentationml/width deck)))
    (is (= "Hello" (:drawingml/text shape)))
    (is (= 28.0 (:drawingml/font-size shape)))
    (is (= "ABCDEF" (get-in deck [:presentationml/theme :presentationml/colors :presentationml.color/accent1])))))

(deftest parses-generated-package-roundtrip
  (let [pkg (pml/package-map {:slides [(pml/slide "<p:sp><p:nvSpPr><p:cNvPr name=\"Generated\"/></p:nvSpPr><p:txBody><a:p><a:r><a:t>Generated text</a:t></a:r></a:p></p:txBody></p:sp>")]})
        deck (parse/deck pkg)]
    (is (parse/valid-deck? deck))
    (is (= "Generated text"
           (-> deck :presentationml/slides first :presentationml/shapes first :drawingml/text)))))
