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
