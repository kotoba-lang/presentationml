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

(defn rels-path [part-path]
  (let [path (str part-path)
        idx (str/last-index-of path "/")]
    (if idx
      (str (subs path 0 idx) "/_rels/" (subs path (inc idx)) ".rels")
      (str "_rels/" path ".rels"))))

(defn- dirname [path]
  (if-let [idx (str/last-index-of (str path) "/")]
    (subs (str path) 0 idx)
    ""))

(defn- normalize-path [path]
  (let [absolute? (str/starts-with? path "/")]
    (->> (str/split (str/replace-first path #"^/" "") #"/")
         (reduce (fn [parts part]
                   (case part
                     "" parts
                     "." parts
                     ".." (vec (butlast parts))
                     (conj parts part)))
                 [])
         (str/join "/")
         (#(if absolute? % %)))))

(defn resolve-target [source-part target]
  (let [target (str target)]
    (cond
      (str/blank? target) target
      (str/starts-with? target "/") (normalize-path target)
      (re-find #"^[A-Za-z][A-Za-z0-9+.-]*:" target) target
      :else (normalize-path (str (dirname source-part) "/" target)))))

(defn relationships [entries part-path]
  (let [rels-xml (entries (rels-path part-path))]
    (into {}
          (keep (fn [match]
                  (let [tag (if (string? match) match (first match))
                        id (dml/xml-attr tag "Id")
                        target (dml/xml-attr tag "Target")]
                    (when id
                      [id (cond-> {:id id
                                   :type (dml/xml-attr tag "Type")
                                   :target target}
                            target (assoc :target-path (resolve-target part-path target)))]))))
          (re-seq #"<Relationship\b[^>]*/?>" (or rels-xml "")))))

(defn- workbook-path [entries chart-part]
  (some (fn [{:keys [target-path target type]}]
          (when (or (some-> target-path (str/ends-with? ".xlsx"))
                    (some-> target (str/ends-with? ".xlsx"))
                    (some-> type (str/includes? "/package")))
            target-path))
        (vals (relationships entries chart-part))))

(defn slide-relationships [entries slide-path]
  (into {}
        (map (fn [[id rel]]
               [id (cond-> rel
                     (some-> (:target-path rel) (str/includes? "/charts/"))
                     (assoc :workbook-path (workbook-path entries (:target-path rel))))]))
        (relationships entries slide-path)))

(defn theme-colors [theme-xml]
  (let [roles ["dk1" "lt1" "dk2" "lt2" "accent1" "accent2" "accent3" "accent4" "accent5" "accent6" "hlink" "folHlink"]]
    (into {}
          (keep (fn [role]
                  (when-let [[_ block] (re-find (re-pattern (str "<a:" role "\\b[^>]*>([\\s\\S]*?)</a:" role ">"))
                                                (or theme-xml ""))]
                    (when-let [color (dml/first-color block)]
                      [(keyword "presentationml.color" role) color]))))
          roles)))

(defn theme-color-roles
  "Plain-keyword {:accent1 \"RRGGBB\" ...} theme colors, for drawingml's
  <a:schemeClr> resolution (drawingml stays decoupled from presentationml's
  namespaced keyword convention)."
  [theme-xml]
  (into {} (map (fn [[k v]] [(keyword (name k)) v])) (theme-colors theme-xml)))

(defn- rel-target-by-type-suffix [entries part-path type-suffix]
  (some (fn [{:keys [type target-path]}]
          (when (and target-path (str/ends-with? (or type "") type-suffix))
            target-path))
        (vals (relationships entries part-path))))

(defn layout-path
  "The slideLayout part path referenced by a slide, via its .rels."
  [entries slide-path]
  (rel-target-by-type-suffix entries slide-path "/slideLayout"))

(defn master-path
  "The slideMaster part path referenced by a slideLayout, via its .rels."
  [entries layout-path]
  (when layout-path
    (rel-target-by-type-suffix entries layout-path "/slideMaster")))

(defn placeholder-geometry
  "Placeholder geometry index for a slide: its slideLayout's placeholder
  positions, falling back to its slideMaster's. Feeds drawingml's
  :placeholder-geometry opt so shapes that omit <a:xfrm> inherit the same
  position PowerPoint itself would apply."
  [entries slide-path]
  (let [layout (layout-path entries slide-path)
        master (master-path entries layout)]
    (dml/merge-placeholder-geometry-indexes
     (some-> layout entries dml/placeholder-geometry-index)
     (some-> master entries dml/placeholder-geometry-index))))

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

(defn slide
  ([idx path xml] (slide idx path xml {}))
  ([idx path xml opts]
  (let [shapes (dml/shapes xml {:part path
                                :rels (:rels opts)
                                :placeholder-geometry (:placeholder-geometry opts)
                                :theme-colors (:theme-colors opts)})]
    {:presentationml/id (str "slide-" (inc idx))
     :presentationml/title (slide-title shapes idx)
     :presentationml/source path
     :presentationml/shapes shapes})))

(defn deck
  ([entries] (deck entries {}))
  ([entries opts]
   (let [presentation (entries "ppt/presentation.xml")
         core (entries "docProps/core.xml")
         size (slide-size presentation)
         theme-color-map (theme-color-roles (entries "ppt/theme/theme1.xml"))
         slide-paths (->> (keys entries)
                          (filter #(re-matches #"ppt/slides/slide\d+\.xml" %))
                          (sort-by slide-number))
         slides (vec (map-indexed (fn [idx path]
                                     (slide idx path (entries path)
                                            {:rels (slide-relationships entries path)
                                             :placeholder-geometry (placeholder-geometry entries path)
                                             :theme-colors theme-color-map}))
                                   slide-paths))
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
