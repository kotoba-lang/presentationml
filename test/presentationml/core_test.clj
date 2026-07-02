(ns presentationml.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [presentationml.core :as pml]
            [presentationml.parse :as parse]))

(deftest renders-slide-and-presentation
  (is (str/includes? (pml/slide-xml (pml/slide "<p:sp/>")) "<p:sp/>"))
  (is (str/includes? (pml/slide-xml (pml/slide {:name "ignored"} "<p:sp/>")) "<p:sp/>"))
  (is (str/includes? (pml/presentation-xml 1 {:width 1280 :height 720}) "<p:sldId id=\"256\""))
  (is (str/includes? (pml/presentation-xml 0 {}) "<p:sldIdLst></p:sldIdLst>"))
  (is (str/includes? (pml/relationships-xml []) "<Relationships"))
  (is (str/includes? (pml/content-types-xml 0) "presentation.xml")))

(deftest builds-package-map
  (let [pkg (pml/package-map {:slides [(pml/slide "<p:sp/>")]})]
    (is (contains? pkg "[Content_Types].xml"))
    (is (contains? pkg "ppt/slides/slide1.xml"))
    (is (str/includes? (get pkg "ppt/_rels/presentation.xml.rels") "slide1.xml"))
    (is (pml/valid-slide? (pml/slide "<p:sp/>")))
    (is (pml/valid-package-map? pkg))
    (is (not (pml/valid-slide? {:presentationml/type :other :body "<p:sp/>"})))
    (is (not (pml/valid-slide? {:presentationml/type :slide :body nil})))
    (is (not (pml/valid-package-map? {})))
    (is (not (pml/valid-package-map? (assoc pkg "ppt/slides/slide1.xml" nil))))))

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

(deftest reads-doc-properties-test
  (testing "every extended docProps/core.xml + docProps/app.xml field, when present"
    (is (= {:presentationml/author "Jun Kawasaki" :presentationml/subject "Q3 Review"
            :presentationml/keywords "quarterly, review" :presentationml/category "Business"
            :presentationml/last-modified-by "Jun Kawasaki"
            :presentationml/created "2026-01-01T00:00:00Z" :presentationml/modified "2026-07-02T00:00:00Z"
            :presentationml/company "GFTD" :presentationml/manager "Someone"}
           (parse/doc-properties
            (str "<cp:coreProperties>"
                 "<dc:creator>Jun Kawasaki</dc:creator><dc:subject>Q3 Review</dc:subject>"
                 "<cp:keywords>quarterly, review</cp:keywords><cp:category>Business</cp:category>"
                 "<cp:lastModifiedBy>Jun Kawasaki</cp:lastModifiedBy>"
                 "<dcterms:created xsi:type=\"dcterms:W3CDTF\">2026-01-01T00:00:00Z</dcterms:created>"
                 "<dcterms:modified xsi:type=\"dcterms:W3CDTF\">2026-07-02T00:00:00Z</dcterms:modified>"
                 "</cp:coreProperties>")
            "<Properties><Company>GFTD</Company><Manager>Someone</Manager></Properties>"))))
  (testing "no extended fields at all -- {}, not nil"
    (is (= {} (parse/doc-properties "<cp:coreProperties><dc:title>Plain</dc:title></cp:coreProperties>" nil))))
  (testing "wired into deck"
    (let [entries {"docProps/core.xml" "<cp:coreProperties><dc:title>T</dc:title><dc:creator>Author</dc:creator></cp:coreProperties>"
                   "docProps/app.xml" "<Properties><Company>Acme</Company></Properties>"
                   "ppt/presentation.xml" "<p:presentation><p:sldSz cx=\"9144000\" cy=\"5143500\"/></p:presentation>"
                   "ppt/slides/slide1.xml" "<p:sld><p:cSld><p:spTree></p:spTree></p:cSld></p:sld>"}
          d (parse/deck entries)]
      (is (= "Author" (:presentationml/author d)))
      (is (= "Acme" (:presentationml/company d))))))

(deftest tracks-distinct-layouts-and-per-slide-layout-ref-test
  (let [entries
        {"ppt/presentation.xml" "<p:presentation><p:sldSz cx=\"9144000\" cy=\"5143500\"/></p:presentation>"

         "ppt/slides/slide1.xml"
         (str "<p:sld><p:cSld><p:spTree>"
              "<p:sp><p:spPr></p:spPr><p:txBody><a:p><a:r><a:t>Title slide</a:t></a:r></a:p></p:txBody></p:sp>"
              "</p:spTree></p:cSld></p:sld>")
         "ppt/slides/_rels/slide1.xml.rels"
         "<Relationships><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout\" Target=\"../slideLayouts/slideLayout1.xml\"/></Relationships>"
         "ppt/slideLayouts/slideLayout1.xml" "<p:sldLayout type=\"title\"><p:cSld><p:spTree></p:spTree></p:cSld></p:sldLayout>"
         "ppt/slideLayouts/_rels/slideLayout1.xml.rels"
         "<Relationships><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideMaster\" Target=\"../slideMasters/slideMaster1.xml\"/></Relationships>"

         "ppt/slides/slide2.xml"
         (str "<p:sld><p:cSld><p:spTree>"
              "<p:sp><p:spPr></p:spPr><p:txBody><a:p><a:r><a:t>Body slide</a:t></a:r></a:p></p:txBody></p:sp>"
              "</p:spTree></p:cSld></p:sld>")
         "ppt/slides/_rels/slide2.xml.rels"
         "<Relationships><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout\" Target=\"../slideLayouts/slideLayout2.xml\"/></Relationships>"
         "ppt/slideLayouts/slideLayout2.xml" "<p:sldLayout type=\"obj\"><p:cSld><p:spTree></p:spTree></p:cSld></p:sldLayout>"
         "ppt/slideLayouts/_rels/slideLayout2.xml.rels"
         "<Relationships><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideMaster\" Target=\"../slideMasters/slideMaster1.xml\"/></Relationships>"

         "ppt/slideMasters/slideMaster1.xml"
         "<p:sldMaster><p:cSld><p:spTree></p:spTree></p:cSld></p:sldMaster>"}
        d (parse/deck entries)
        layouts (:presentationml/layouts d)
        slide1 (first (:presentationml/slides d))
        slide2 (second (:presentationml/slides d))]
    (testing "two distinct layouts are tracked, each with its own type -- even though BOTH use the same master"
      (is (= 2 (count layouts)))
      (is (= #{"title" "obj"} (set (keep :presentationml/layout-type layouts)))))
    (testing "each slide is tagged with the id of the layout it actually uses"
      (is (some? (:presentationml/layout-ref slide1)))
      (is (some? (:presentationml/layout-ref slide2)))
      (is (not= (:presentationml/layout-ref slide1) (:presentationml/layout-ref slide2)))))
  (testing "a single-layout deck gets NO :presentationml/layouts key at all (unchanged behavior)"
    (let [entries {"ppt/slides/slide1.xml" "<p:sld><p:cSld><p:spTree></p:spTree></p:cSld></p:sld>"}
          d (parse/deck entries)]
      (is (not (contains? d :presentationml/layouts)))
      (is (not (contains? (first (:presentationml/slides d)) :presentationml/layout-ref))))))

(deftest reads-slide-transition-test
  (testing "a transition with an effect element, speed, and explicit advance-on-click=false + timed advance"
    (let [sld-xml (str "<p:sld><p:cSld><p:spTree></p:spTree></p:cSld>"
                        "<p:transition spd=\"slow\" advClick=\"0\" advTm=\"3000\"><p:wipe dir=\"l\"/></p:transition>"
                        "</p:sld>")]
      (is (= {:speed "slow" :advance-on-click false :advance-after-time 3000
              :type "wipe" :attrs {"dir" "l"}}
             (parse/transition sld-xml)))))
  (testing "a bare self-closing <p:transition/> (no effect element, no attrs) -- an empty map, not nil"
    (is (= {} (parse/transition "<p:sld><p:cSld></p:cSld><p:transition/></p:sld>"))))
  (testing "a transition with only timing attrs and no effect child"
    (is (= {:speed "fast"}
           (parse/transition "<p:sld><p:cSld></p:cSld><p:transition spd=\"fast\"/></p:sld>"))))
  (testing "no <p:transition> at all -- nil, the overwhelming common case"
    (is (nil? (parse/transition "<p:sld><p:cSld><p:spTree></p:spTree></p:cSld></p:sld>"))))
  (testing "wired into slide -- :presentationml/transition present only when the slide XML has one"
    (is (= {:type "fade"}
           (:presentationml/transition
            (parse/slide 0 "ppt/slides/slide1.xml"
                         (str "<p:sld><p:cSld><p:spTree></p:spTree></p:cSld>"
                              "<p:transition><p:fade/></p:transition></p:sld>")))))
    (is (nil? (:presentationml/transition
               (parse/slide 0 "ppt/slides/slide1.xml"
                            "<p:sld><p:cSld><p:spTree></p:spTree></p:cSld></p:sld>"))))))

(deftest extracts-speaker-notes-from-notes-slide-part
  (let [entries
        {"ppt/presentation.xml" "<p:presentation><p:sldSz cx=\"9144000\" cy=\"5143500\"/></p:presentation>"
         "ppt/slides/slide1.xml"
         (str "<p:sld><p:cSld><p:spTree>"
              "<p:sp><p:nvSpPr><p:cNvPr id=\"2\" name=\"Title\"/></p:nvSpPr>"
              "<p:txBody><a:p><a:r><a:t>Slide with notes</a:t></a:r></a:p></p:txBody></p:sp>"
              "</p:spTree></p:cSld></p:sld>")
         "ppt/slides/_rels/slide1.xml.rels"
         (str "<Relationships>"
              "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/notesSlide\" Target=\"../notesSlides/notesSlide1.xml\"/>"
              "</Relationships>")
         "ppt/notesSlides/notesSlide1.xml"
         (str "<p:notes><p:cSld><p:spTree>"
              "<p:sp><p:nvSpPr><p:nvPr><p:ph type=\"sldImg\"/></p:nvPr></p:nvSpPr></p:sp>"
              "<p:sp><p:nvSpPr><p:nvPr><p:ph type=\"body\"/></p:nvPr></p:nvSpPr>"
              "<p:txBody><a:p><a:r><a:t>Remember to mention Q4 numbers</a:t></a:r></a:p></p:txBody></p:sp>"
              "</p:spTree></p:cSld></p:notes>")}
        deck (parse/deck entries)
        slide (-> deck :presentationml/slides first)]
    (is (= "Remember to mention Q4 numbers" (:presentationml/notes slide))))
  (testing "a slide with no notesSlide relationship simply has no :presentationml/notes key"
    (let [entries {"ppt/slides/slide1.xml" "<p:sld><p:cSld><p:spTree></p:spTree></p:cSld></p:sld>"}
          deck (parse/deck entries)]
      (is (not (contains? (-> deck :presentationml/slides first) :presentationml/notes))))))

(deftest tracks-distinct-masters-and-per-slide-master-ref-for-multi-master-decks
  (let [entries
        {"ppt/presentation.xml" "<p:presentation><p:sldSz cx=\"9144000\" cy=\"5143500\"/></p:presentation>"
         "ppt/theme/theme1.xml" "<a:theme><a:clrScheme><a:dk1><a:srgbClr val=\"111111\"/></a:dk1></a:clrScheme></a:theme>"

         "ppt/slides/slide1.xml"
         (str "<p:sld><p:cSld><p:spTree>"
              "<p:sp><p:spPr></p:spPr><p:txBody><a:p><a:r><a:t>Dark section</a:t></a:r></a:p></p:txBody></p:sp>"
              "</p:spTree></p:cSld></p:sld>")
         "ppt/slides/_rels/slide1.xml.rels"
         (str "<Relationships><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout\" Target=\"../slideLayouts/slideLayout1.xml\"/></Relationships>")
         "ppt/slideLayouts/slideLayout1.xml" "<p:sldLayout><p:cSld><p:spTree></p:spTree></p:cSld></p:sldLayout>"
         "ppt/slideLayouts/_rels/slideLayout1.xml.rels"
         "<Relationships><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideMaster\" Target=\"../slideMasters/slideMaster1.xml\"/></Relationships>"
         "ppt/slideMasters/slideMaster1.xml"
         "<p:sldMaster><p:cSld><p:bg><p:bgPr><a:solidFill><a:srgbClr val=\"222222\"/></a:solidFill></p:bgPr></p:bg><p:spTree></p:spTree></p:cSld></p:sldMaster>"

         "ppt/slides/slide2.xml"
         (str "<p:sld><p:cSld><p:spTree>"
              "<p:sp><p:spPr></p:spPr><p:txBody><a:p><a:r><a:t>Light section</a:t></a:r></a:p></p:txBody></p:sp>"
              "</p:spTree></p:cSld></p:sld>")
         "ppt/slides/_rels/slide2.xml.rels"
         (str "<Relationships><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout\" Target=\"../slideLayouts/slideLayout2.xml\"/></Relationships>")
         "ppt/slideLayouts/slideLayout2.xml" "<p:sldLayout><p:cSld><p:spTree></p:spTree></p:cSld></p:sldLayout>"
         "ppt/slideLayouts/_rels/slideLayout2.xml.rels"
         "<Relationships><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideMaster\" Target=\"../slideMasters/slideMaster2.xml\"/></Relationships>"
         "ppt/slideMasters/slideMaster2.xml"
         "<p:sldMaster><p:cSld><p:bg><p:bgPr><a:solidFill><a:srgbClr val=\"EEEEEE\"/></a:solidFill></p:bgPr></p:bg><p:spTree></p:spTree></p:cSld></p:sldMaster>"}
        d (parse/deck entries)
        masters (:presentationml/masters d)
        slide1 (first (:presentationml/slides d))
        slide2 (second (:presentationml/slides d))]
    (testing "two distinct masters are tracked, each with its own background"
      (is (= 2 (count masters)))
      (is (= #{"222222" "EEEEEE"} (set (keep :presentationml/background masters)))))
    (testing "each slide is tagged with the id of the master it actually uses"
      (is (some? (:presentationml/master-ref slide1)))
      (is (some? (:presentationml/master-ref slide2)))
      (is (not= (:presentationml/master-ref slide1) (:presentationml/master-ref slide2)))))
  (testing "a single-master deck gets NO :presentationml/masters key at all (unchanged behavior)"
    (let [entries {"ppt/slides/slide1.xml" "<p:sld><p:cSld><p:spTree></p:spTree></p:cSld></p:sld>"}
          d (parse/deck entries)]
      (is (not (contains? d :presentationml/masters)))
      (is (not (contains? (first (:presentationml/slides d)) :presentationml/master-ref))))))

(deftest resolves-schemeclr-through-a-slide-level-clrmapovr
  (let [entries
        {"ppt/presentation.xml" "<p:presentation><p:sldSz cx=\"9144000\" cy=\"5143500\"/></p:presentation>"
         "ppt/theme/theme1.xml" "<a:theme><a:clrScheme><a:dk1><a:srgbClr val=\"111111\"/></a:dk1><a:dk2><a:srgbClr val=\"222222\"/></a:dk2></a:clrScheme></a:theme>"
         "ppt/slides/slide1.xml"
         (str "<p:sld><p:cSld><p:spTree>"
              "<p:sp><p:spPr></p:spPr>"
              "<p:txBody><a:p><a:r><a:rPr><a:solidFill><a:schemeClr val=\"tx1\"/></a:solidFill></a:rPr><a:t>Slide-level override</a:t></a:r></a:p></p:txBody></p:sp>"
              "</p:spTree></p:cSld>"
              "<p:clrMapOvr><a:overrideClrMapping bg1=\"lt1\" tx1=\"dk2\" bg2=\"lt1\" tx2=\"dk1\" accent1=\"accent1\" accent2=\"accent2\" accent3=\"accent3\" accent4=\"accent4\" accent5=\"accent5\" accent6=\"accent6\" hlink=\"hlink\" folHlink=\"folHlink\"/></p:clrMapOvr>"
              "</p:sld>")
         "ppt/slides/_rels/slide1.xml.rels"
         "<Relationships><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout\" Target=\"../slideLayouts/slideLayout1.xml\"/></Relationships>"
         "ppt/slideLayouts/slideLayout1.xml" "<p:sldLayout><p:cSld><p:spTree></p:spTree></p:cSld></p:sldLayout>"
         "ppt/slideLayouts/_rels/slideLayout1.xml.rels"
         "<Relationships><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideMaster\" Target=\"../slideMasters/slideMaster1.xml\"/></Relationships>"
         ;; master's OWN clrMap keeps the default tx1->dk1; the SLIDE's own clrMapOvr should win over this.
         "ppt/slideMasters/slideMaster1.xml"
         "<p:sldMaster><p:clrMap bg1=\"lt1\" tx1=\"dk1\" bg2=\"lt1\" tx2=\"dk2\" accent1=\"accent1\" accent2=\"accent2\" accent3=\"accent3\" accent4=\"accent4\" accent5=\"accent5\" accent6=\"accent6\" hlink=\"hlink\" folHlink=\"folHlink\"/></p:sldMaster>"}
        d (parse/deck entries)
        shape (-> d :presentationml/slides first :presentationml/shapes first)]
    (is (= "222222" (:drawingml/color shape))
        "the slide's own clrMapOvr (tx1->dk2) wins over the master's default clrMap (tx1->dk1)"))
  (testing "a slide with <a:masterClrMapping/> (the common case) still inherits the master's clrMap unchanged"
    (let [entries
          {"ppt/theme/theme1.xml" "<a:theme><a:clrScheme><a:dk1><a:srgbClr val=\"111111\"/></a:dk1></a:clrScheme></a:theme>"
           "ppt/slides/slide1.xml"
           (str "<p:sld><p:cSld><p:spTree>"
                "<p:sp><p:spPr></p:spPr>"
                "<p:txBody><a:p><a:r><a:rPr><a:solidFill><a:schemeClr val=\"tx1\"/></a:solidFill></a:rPr><a:t>Inherited</a:t></a:r></a:p></p:txBody></p:sp>"
                "</p:spTree></p:cSld><p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:sld>")}
          d (parse/deck entries)
          shape (-> d :presentationml/slides first :presentationml/shapes first)]
      (is (= "111111" (:drawingml/color shape))))))

(deftest resolves-schemeclr-through-a-custom-non-default-clrmap
  (let [entries
        {"ppt/presentation.xml" "<p:presentation><p:sldSz cx=\"9144000\" cy=\"5143500\"/></p:presentation>"
         "ppt/theme/theme1.xml" "<a:theme><a:clrScheme><a:dk1><a:srgbClr val=\"111111\"/></a:dk1><a:lt1><a:srgbClr val=\"EEEEEE\"/></a:lt1><a:dk2><a:srgbClr val=\"222222\"/></a:dk2></a:clrScheme></a:theme>"
         "ppt/slides/slide1.xml"
         (str "<p:sld><p:cSld><p:spTree>"
              "<p:sp><p:spPr></p:spPr>"
              "<p:txBody><a:p><a:r><a:rPr><a:solidFill><a:schemeClr val=\"tx1\"/></a:solidFill></a:rPr><a:t>Custom clrMap text</a:t></a:r></a:p></p:txBody></p:sp>"
              "</p:spTree></p:cSld></p:sld>")
         "ppt/slides/_rels/slide1.xml.rels"
         (str "<Relationships>"
              "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout\" Target=\"../slideLayouts/slideLayout1.xml\"/>"
              "</Relationships>")
         "ppt/slideLayouts/slideLayout1.xml" "<p:sldLayout><p:cSld><p:spTree></p:spTree></p:cSld></p:sldLayout>"
         "ppt/slideLayouts/_rels/slideLayout1.xml.rels"
         (str "<Relationships>"
              "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideMaster\" Target=\"../slideMasters/slideMaster1.xml\"/>"
              "</Relationships>")
         ;; a NON-default clrMap: tx1 points to dk2 (222222), not the usual dk1 (111111).
         "ppt/slideMasters/slideMaster1.xml"
         "<p:sldMaster><p:clrMap bg1=\"lt1\" tx1=\"dk2\" bg2=\"lt1\" tx2=\"dk1\" accent1=\"accent1\" accent2=\"accent2\" accent3=\"accent3\" accent4=\"accent4\" accent5=\"accent5\" accent6=\"accent6\" hlink=\"hlink\" folHlink=\"folHlink\"/></p:sldMaster>"}
        deck (parse/deck entries)
        shape (-> deck :presentationml/slides first :presentationml/shapes first)]
    (is (= "222222" (:drawingml/color shape))
        "tx1 resolves through the master's OWN clrMap (->dk2), not the OOXML default (->dk1)"))
  (testing "no <p:clrMap> at all on the master -- falls back to the OOXML default map, unchanged from before"
    (let [entries
          {"ppt/theme/theme1.xml" "<a:theme><a:clrScheme><a:dk1><a:srgbClr val=\"111111\"/></a:dk1></a:clrScheme></a:theme>"
           "ppt/slides/slide1.xml"
           (str "<p:sld><p:cSld><p:spTree>"
                "<p:sp><p:spPr></p:spPr>"
                "<p:txBody><a:p><a:r><a:rPr><a:solidFill><a:schemeClr val=\"tx1\"/></a:solidFill></a:rPr><a:t>Default clrMap text</a:t></a:r></a:p></p:txBody></p:sp>"
                "</p:spTree></p:cSld></p:sld>")}
          deck (parse/deck entries)
          shape (-> deck :presentationml/slides first :presentationml/shapes first)]
      (is (= "111111" (:drawingml/color shape))))))

(deftest inherits-placeholder-geometry-and-scheme-color-from-layout
  (let [entries
        {"ppt/presentation.xml" "<p:presentation><p:sldSz cx=\"9144000\" cy=\"5143500\"/></p:presentation>"
         "ppt/theme/theme1.xml" "<a:theme><a:clrScheme><a:accent1><a:srgbClr val=\"4472C4\"/></a:accent1></a:clrScheme></a:theme>"
         "ppt/slides/slide1.xml"
         (str "<p:sld><p:cSld><p:spTree>"
              "<p:sp><p:nvSpPr><p:nvPr><p:ph type=\"title\"/></p:nvPr></p:nvSpPr>"
              "<p:spPr></p:spPr>"
              "<p:txBody><a:p><a:r><a:rPr><a:solidFill><a:schemeClr val=\"accent1\"/></a:solidFill></a:rPr><a:t>Untouched title</a:t></a:r></a:p></p:txBody></p:sp>"
              "</p:spTree></p:cSld></p:sld>")
         "ppt/slides/_rels/slide1.xml.rels"
         (str "<Relationships>"
              "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout\" Target=\"../slideLayouts/slideLayout1.xml\"/>"
              "</Relationships>")
         "ppt/slideLayouts/slideLayout1.xml"
         (str "<p:sldLayout><p:cSld><p:spTree>"
              "<p:sp><p:nvSpPr><p:nvPr><p:ph type=\"title\"/></p:nvPr></p:nvSpPr>"
              "<p:spPr><a:xfrm><a:off x=\"457200\" y=\"274638\"/><a:ext cx=\"8229600\" cy=\"1143000\"/></a:xfrm></p:spPr></p:sp>"
              "</p:spTree></p:cSld></p:sldLayout>")}
        deck (parse/deck entries)
        shape (-> deck :presentationml/slides first :presentationml/shapes first)]
    ;; The slide's own <p:sp> omits <a:xfrm> (as PowerPoint does for an
    ;; untouched placeholder); position must come from the layout, not the
    ;; hardcoded 0.8in/0.8in/8.4in/0.7in fallback.
    (is (= 0.5 (:drawingml/x shape)))
    (is (not= 0.8 (:drawingml/x shape)))
    ;; <a:schemeClr val="accent1"/> resolves through the theme, not the
    ;; hardcoded "17202A" fallback.
    (is (= "4472C4" (:drawingml/color shape)))))

(deftest parses-generated-package-roundtrip
  (let [pkg (pml/package-map {:slides [(pml/slide "<p:sp><p:nvSpPr><p:cNvPr name=\"Generated\"/></p:nvSpPr><p:txBody><a:p><a:r><a:t>Generated text</a:t></a:r></a:p></p:txBody></p:sp>")]})
        deck (parse/deck pkg)]
    (is (parse/valid-deck? deck))
    (is (= "Generated text"
           (-> deck :presentationml/slides first :presentationml/shapes first :drawingml/text)))))

(deftest theme-fonts-extracts-east-asian-and-complex-script-typefaces
  (let [theme-xml (str "<a:theme><a:themeElements><a:fontScheme>"
                       "<a:majorFont><a:latin typeface=\"Aptos Display\"/>"
                       "<a:ea typeface=\"游ゴシック\"/><a:cs typeface=\"Arial\"/></a:majorFont>"
                       "<a:minorFont><a:latin typeface=\"Aptos\"/>"
                       "<a:ea typeface=\"メイリオ\"/><a:cs typeface=\"Arial\"/></a:minorFont>"
                       "</a:fontScheme></a:themeElements></a:theme>")]
    (is (= {:presentationml.font/majorFont "Aptos Display"
            :presentationml.font/majorFont-ea "游ゴシック"
            :presentationml.font/majorFont-cs "Arial"
            :presentationml.font/minorFont "Aptos"
            :presentationml.font/minorFont-ea "メイリオ"
            :presentationml.font/minorFont-cs "Arial"}
           (parse/theme-fonts theme-xml))))
  (testing "an empty ea/cs typeface attribute (PowerPoint often emits typeface=\"\") is treated as absent"
    (let [theme-xml (str "<a:theme><a:themeElements><a:fontScheme>"
                         "<a:majorFont><a:latin typeface=\"Aptos Display\"/><a:ea typeface=\"\"/><a:cs typeface=\"\"/></a:majorFont>"
                         "<a:minorFont><a:latin typeface=\"Aptos\"/><a:ea typeface=\"\"/><a:cs typeface=\"\"/></a:minorFont>"
                         "</a:fontScheme></a:themeElements></a:theme>")]
      (is (= {:presentationml.font/majorFont "Aptos Display" :presentationml.font/minorFont "Aptos"}
             (parse/theme-fonts theme-xml))))))

(deftest parses-defaults-and-edge-cases
  (is (= {:presentationml/width 10.0 :presentationml/height 5.625}
         (parse/slide-size nil)))
  (is (= 0 (parse/slide-number "ppt/slides/slidebad.xml")))
  (is (= 12 (parse/slide-number "ppt/slides/slide12.xml")))
  (is (= {} (parse/theme {})))
  (is (= {:presentationml.font/majorFont "Aptos Display"
          :presentationml.font/minorFont "Aptos"}
         (parse/theme-fonts nil)))
  (is (= {:presentationml/source "ppt/theme/theme1.xml"
          :presentationml/fonts {:presentationml.font/majorFont "Aptos Display"
                                 :presentationml.font/minorFont "Aptos"}}
         (parse/theme {"ppt/theme/theme1.xml" "<a:theme/>"})))
  (is (= "Slide 3" (parse/slide-title [] 2)))
  (is (= "From shape" (parse/slide-title [{:drawingml/text "From shape"}] 0)))
  (let [deck (parse/deck {"ppt/presentation.xml" "<p:presentation/>"} {:presentationml/title "Attr title"})]
    (is (parse/valid-deck? deck))
    (is (= "Attr title" (:presentationml/title deck)))
    (is (= "slide-1" (-> deck :presentationml/slides first :presentationml/id))))
  (let [deck (parse/deck {} {:title "Explicit title"})]
    (is (= "Explicit title" (:presentationml/title deck))))
  (let [deck (parse/deck {})]
    (is (= "Imported deck" (:presentationml/title deck))))
  (is (nil? (parse/parse-long-safe nil)))
  (is (not (parse/valid-deck? {:presentationml/id 1 :presentationml/slides []})))
  (is (not (parse/valid-deck? nil)))
  (is (not (parse/valid-deck? {:presentationml/id "x" :presentationml/slides [{:presentationml/shapes nil}]}))))

(deftest resolves-slide-chart-and-workbook-relationships
  (let [entries {"ppt/slides/slide1.xml" "<p:sld><p:cSld><p:spTree><p:graphicFrame><p:nvGraphicFramePr><p:cNvPr name=\"Revenue\"/></p:nvGraphicFramePr><a:graphic><a:graphicData><c:chart r:id=\"rId2\"/></a:graphicData></a:graphic></p:graphicFrame></p:spTree></p:cSld></p:sld>"
                 "ppt/slides/_rels/slide1.xml.rels" "<Relationships><Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/chart\" Target=\"../charts/chart1.xml\"/></Relationships>"
                 "ppt/charts/chart1.xml" "<c:chartSpace/>"
                 "ppt/charts/_rels/chart1.xml.rels" "<Relationships><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/package\" Target=\"../embeddings/Microsoft_Excel_Worksheet1.xlsx\"/></Relationships>"
                 "ppt/embeddings/Microsoft_Excel_Worksheet1.xlsx" "workbook"}
        rels (parse/slide-relationships entries "ppt/slides/slide1.xml")
        deck (parse/deck entries)
        chart (-> deck :presentationml/slides first :presentationml/shapes first)]
    (is (= "ppt/charts/chart1.xml" (get-in rels ["rId2" :target-path])))
    (is (= "ppt/embeddings/Microsoft_Excel_Worksheet1.xlsx"
           (get-in rels ["rId2" :workbook-path])))
    (is (= "ppt/charts/chart1.xml" (:drawingml/chart-part chart)))
    (is (= "ppt/embeddings/Microsoft_Excel_Worksheet1.xlsx"
           (:drawingml/workbook-part chart)))))

(deftest master-background-gradient-test
  (testing "a multi-stop gradient background carries its FULL fidelity, not just the first stop"
    (let [master-xml (str "<p:sldMaster><p:cSld><p:bg><p:bgPr><a:gradFill><a:gsLst>"
                          "<a:gs pos=\"0\"><a:srgbClr val=\"112233\"/></a:gs>"
                          "<a:gs pos=\"100000\"><a:srgbClr val=\"445566\"/></a:gs>"
                          "</a:gsLst><a:lin ang=\"5400000\"/></a:gradFill></p:bgPr></p:bg>"
                          "<p:spTree></p:spTree></p:cSld></p:sldMaster>")]
      (is (= {:stops [[0.0 "112233"] [100.0 "445566"]] :angle 90.0}
             (parse/master-background master-xml nil)))))
  (testing "a plain solid background is still a flat hex string, unchanged"
    (let [master-xml "<p:sldMaster><p:cSld><p:bg><p:bgPr><a:solidFill><a:srgbClr val=\"336699\"/></a:solidFill></p:bgPr></p:bg><p:spTree></p:spTree></p:cSld></p:sldMaster>"]
      (is (= "336699" (parse/master-background master-xml nil)))))
  (testing "no <p:bg> at all -- nil"
    (is (nil? (parse/master-background "<p:sldMaster><p:cSld><p:spTree></p:spTree></p:cSld></p:sldMaster>" nil)))))
