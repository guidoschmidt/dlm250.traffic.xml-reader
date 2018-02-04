(ns xml.core
  (:require [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zx]))

;; -- XML/Data file definitions
(def in-folder "c:/Users/gs/Google Drive/Projects/2018/Logo Movepod/data/dlm250.utm32s.nas_bda.kompakt/dlm250-aaa_kompakt/")
(def in-file-example "BDA_42003-Example.xml")
(def in-file-strasse "BDA_42002-Strasse.xml")
(def in-file-strassenachse "BDA_42003-Strassenachse.xml")
(def in-file-bahnstrecke "BDA_42014-Bahnstrecke.xml")
(def in-file-flugverkehr "BDA_42015-Flugverkehr.xml")
;; (def in-file-wasserlauf "BDA_44002-Wasserlauf.xml")
(def in-file-schiffsfahrtslinie "BDA_57002-Schifffahrtslinie.xml")

;; -- Attributes
(def BDA_42003_autobahn 1301)
(def BDA_42003_bundesstrasse 1303)
(def BDA_42014_eisenbahn 1100)
(def BDA_42014_gueterverkehr 1102)
(def BDA_42014_sbahn 1104)
(def BDA_42015_flughafen 5510)
(def BDA_42015_flughafen_intl 5511)
(def BDA_57002_autofaehrverkehr 1710)
(def BDA_57002_eisenbahnfaerverkehr 1720)
(def BDA_57002_personenfaehrverkehr 1730)

;; -- Utility functions
(defn recur-to-tag [tag elem]
  (if (or (= elem nil)
          (= tag (:tag elem)))
    elem
    (recur tag (first (:content elem)))))

(defn find-tag [tag tags]
  (filter #(= tag (:tag %)) tags))

(defn strip-head [xml]
  (->> xml
       :content
       (drop 5)
       first ;; enthaelt
       :content
       first ;; FeatureCollection
       :content
       ;; DEBUG: take only x elements
       ;; (take 5)
       ))

;; -- XML IO
(defn save-xml [filename root content]
  (println content)
  (let [tags (xml/element root { :xml:lang "de" } content)]
    (with-open [out-file (clojure.java.io/writer filename)]
      (xml/emit tags out-file))))

;; -- Strassen
(defn subs-identifier [elem]
  (subs elem 12))

(defn extract-posList [elem]
  (->> (filter #(= :position (:tag %)) (:content (recur-to-tag :AX_Strassenachse elem)))
       first
       :content
       (map #(recur-to-tag :posList %))))

(def autobahn-identifiers
  (let [xml-file (str in-folder in-file-strasse)
        input (clojure.java.io/reader (clojure.java.io/file xml-file))]
    (->> (xml/parse input)
         strip-head
         (map #(recur-to-tag :AX_Strasse %))
         (filter #(->> (:content (recur-to-tag :AX_Strasse %))
                       (find-tag :widmung)
                       first
                       :content
                       first
                       read-string
                       (= BDA_42003_autobahn)))
         ;; (take 1) ;; DEBUG
         (map #(->> (:content (recur-to-tag :AX_Strasse %))
                    (find-tag :identifier)
                    first
                    :content
                    first
                    subs-identifier)))))

(defn extract-autobahn-ids []
  "Extract all identifiers that match to BDA_42003_autobahn to xml."
  (->> autobahn-identifiers
       (map #(xml/element :identifier {} %))))

(defn extract-strassenachsen []
  "Extract all positons (posList) tags that are linked to autobahn (BDA_42003_autobahn)."
  (let [xml-file (str in-folder in-file-strassenachse)
        reader (clojure.java.io/reader (clojure.java.io/file xml-file))]
    (->> (xml/parse reader)
         strip-head
         (filter #(->> (:content (recur-to-tag :AX_Strassenachse %))
                       (find-tag :istTeilVon)
                       first
                       :attrs
                       :xlink/href
                       subs-identifier
                       (.contains (lazy-seq autobahn-identifiers))))
         (map #(extract-posList %)))))

;; -- Bahnverkehr
(defn match-bahnverkehr-p [elem]
  (or (= BDA_42014_eisenbahn elem)
      (= BDA_42014_gueterverkehr elem)))

(defn extract-bahnverkehr []
  (let [xml-file (str in-folder in-file-bahnstrecke)
        reader (clojure.java.io/reader (clojure.java.io/file xml-file))]
    (->> (xml/parse reader)
         strip-head
         (filter #(->> (:content (recur-to-tag :AX_Bahnstrecke %))
                       (find-tag :bahnkategorie)
                       first
                       :content
                       first
                       read-string
                       match-bahnverkehr-p))
         (map #(->> (recur-to-tag :AX_Bahnstrecke %)
                    :content
                    (find-tag :position)
                    first
                    (recur-to-tag :posList))))))

;; -- Flugverkehr
(defn match-flugverkehr-p [elem]
  (or (= BDA_42015_flughafen elem)
      (= BDA_42015_flughafen_intl elem)))

(defn extract-flugverkehr []
  (let [xml-file (str in-folder in-file-flugverkehr)
        reader (clojure.java.io/reader (clojure.java.io/file xml-file))]
    (->> (xml/parse reader)
         strip-head
         ;; Skip filtering of Flugverkehr, as the data is narrow enough
         ;; (map #(->> (:content (recur-to-tag :AX_Flugverkehr %))
         ;;               (find-tag :art)
         ;;               first
         ;;               :content
         ;;               first
         ;;               read-string
         ;;               match-flugverkehr-p))
         (map #(->> (recur-to-tag :AX_Flugverkehr %)
                    :content
                    (find-tag :position)
                    first
                    (recur-to-tag :posList))))))

;; -- Schiffsverkehr
(defn match-schiffsverkehr-p [elem]
  (or (= BDA_57002_autofaehrverkehr elem)
      (= BDA_57002_eisenbahnfaerverkehr elem)
      (= BDA_57002_personenfaehrverkehr elem)))

(defn extract-schiffsverkehr []
  (let [xml-file (str in-folder in-file-schiffsfahrtslinie)
        reader (clojure.java.io/reader (clojure.java.io/file xml-file))]
    (->> (xml/parse reader)
         strip-head
         (filter #(->> (:content (recur-to-tag :AX_SchifffahrtslinieFaehrverkehr %))
                       (find-tag :art)
                       first
                       :content
                       first
                       read-string
                       match-schiffsverkehr-p))
         (map #(->> (recur-to-tag :AX_SchifffahrtslinieFaehrverkehr %)
                    :content
                    (find-tag :position)
                    first
                    (recur-to-tag :posList))))))

;; -- Main
(defn -main []
  (save-xml "out/autobahn.ids.xml" :autobahn-ids (extract-autobahn-ids))
  (save-xml "out/strassenachsen.xml" :strassenachsen (extract-strassenachsen))
  (save-xml "out/bahnverkehr.xml" :bahnverkehr (extract-bahnverkehr))
  (save-xml "out/flugverkehr.xml" :flugverkehr (extract-flugverkehr))
  (save-xml "out/schiffsverkehr.xml" :schiffsverkehr (extract-schiffsverkehr)))
