(ns presentationml.parse
  "Small PresentationML package XML to EDN projection helpers."
  (:require [clojure.string :as str]
            [drawingml.parse :as dml]))

(def default-width 10.0)
(def default-height 5.625)

(defn parse-long-safe [x]
  (when (and x (re-matches #"-?\d+" (str x)))
    #?(:clj (Long/parseLong (str x))
       :cljs (js/parseInt (str x) 10))))

(defn slide-size [xml]
  (if-let [sld (re-find #"<p:sldSz\b[^>]*>" (or xml ""))]
    {:presentationml/width (dml/emu->inch (dml/xml-attr sld "cx") default-width)
     :presentationml/height (dml/emu->inch (dml/xml-attr sld "cy") default-height)}
    {:presentationml/width default-width
     :presentationml/height default-height}))

(defn slide-number [name]
  (or (some-> (second (re-find #"slide(\d+)\.xml$" (str name))) parse-long-safe)
      0))

(defn theme-colors [theme-xml]
  (let [roles ["dk1" "lt1" "dk2" "lt2" "accent1" "accent2" "accent3" "accent4" "accent5" "accent6" "hlink" "folHlink"]]
    (into {}
          (keep (fn [role]
                  (when-let [[_ block] (re-find (re-pattern (str "<a:" role "\\b[^>]*>([\\s\\S]*?)</a:" role ">"))
                                                (or theme-xml ""))]
                    (when-let [color (dml/first-color block)]
                      [(keyword "presentationml.color" role) color]))))
          roles)))

(defn theme-fonts [theme-xml]
  (let [major (or (some-> (second (re-find #"<a:majorFont>[\s\S]*?<a:latin\b[^>]*typeface=\"([^\"]+)\"" (or theme-xml ""))) dml/xml-unescape)
                  "Aptos Display")
        minor (or (some-> (second (re-find #"<a:minorFont>[\s\S]*?<a:latin\b[^>]*typeface=\"([^\"]+)\"" (or theme-xml ""))) dml/xml-unescape)
                  "Aptos")]
    {:presentationml.font/majorFont major
     :presentationml.font/minorFont minor}))

(defn theme [entries]
  (let [theme-xml (entries "ppt/theme/theme1.xml")
        colors (theme-colors theme-xml)
        fonts (theme-fonts theme-xml)]
    (if theme-xml
      (cond-> {:presentationml/source "ppt/theme/theme1.xml"}
        (seq colors) (assoc :presentationml/colors colors)
        (seq fonts) (assoc :presentationml/fonts fonts))
      {})))

(defn slide-title [shapes idx]
  (or (some :drawingml/text shapes)
      (str "Slide " (inc idx))))

(defn slide [idx path xml]
  (let [shapes (dml/shapes xml {:part path})]
    {:presentationml/id (str "slide-" (inc idx))
     :presentationml/title (slide-title shapes idx)
     :presentationml/source path
     :presentationml/shapes shapes}))

(defn deck
  ([entries] (deck entries {}))
  ([entries opts]
   (let [presentation (entries "ppt/presentation.xml")
         core (entries "docProps/core.xml")
         size (slide-size presentation)
         slide-paths (->> (keys entries)
                          (filter #(re-matches #"ppt/slides/slide\d+\.xml" %))
                          (sort-by slide-number))
         slides (vec (map-indexed (fn [idx path] (slide idx path (entries path))) slide-paths))
         title (or (:title opts)
                   (:presentationml/title opts)
                   (dml/first-xml-text core "dc:title")
                   "Imported deck")
         theme (theme entries)]
     (merge {:presentationml/id "imported-pptx"
             :presentationml/title title
             :presentationml/import {:presentationml/format :pptx
                                     :presentationml/text-extraction :drawingml-xml}
             :presentationml/slides (if (seq slides)
                                      slides
                                      [{:presentationml/id "slide-1"
                                        :presentationml/title title
                                        :presentationml/shapes []}])}
            size
            (when (seq theme) {:presentationml/theme theme})))))

(defn valid-deck? [deck]
  (and (map? deck)
       (string? (:presentationml/id deck))
       (sequential? (:presentationml/slides deck))
       (every? #(sequential? (:presentationml/shapes %)) (:presentationml/slides deck))))
