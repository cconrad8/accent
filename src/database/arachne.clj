(ns database.arachne
  (:gen-class)
  (:require [arachne.aristotle :as aa]
            [arachne.aristotle.registry :as reg]
            [arachne.aristotle.query :as q]
            [clojure.java.io :as io])
  (:import [java.io File]
           [org.apache.jena.util FileManager LocationMapper]))  ;; <- this was missing a closing paren

(defn disable-remote-resolution! []
  (let [fm (FileManager/get)
        lm (.getLocationMapper fm)]
    (.addAltEntry lm "http://purl.org/dc/terms/" "file:resources/rdf/vocab/dcterms.ttl")
    (.addAltEntry lm "http://www.w3.org/2000/01/rdf-schema#" "file:resources/rdf/vocab/rdfs.ttl")
    (.addAltEntry lm "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "file:resources/rdf/vocab/rdf.ttl")))

(defn get-all-turtle-files
  "Get all turtle files from resources directory"
  []
  (let [resources-dir (File. "resources/rdf")
        exists? (.exists resources-dir)
        is-dir? (.isDirectory resources-dir)]
    (if (and exists? is-dir?)
      (->> (.listFiles resources-dir)
           (filter #(.endsWith (.getName %) ".ttl"))
           (map #(.getAbsolutePath %)))
      (do
        (println "Resources directory not found or not a directory")
        []))))

(defn load-all-turtle-files
  "Create a Jena-mini graph and load all ttl files from directory"
  []
  (let [graph (aa/graph :jena-mini)
        ttl (get-all-turtle-files)]
    (if (empty? ttl)
      (println "No .ttl files found!")
      (do
        (println (str "Loading " (count ttl) " resource files into graph..."))
        (doseq [file ttl]
          (println (str "Loading " file))
          (aa/read graph file))))
    graph))

(def graph (load-all-turtle-files))

(defn count-triples [graph]
  (count (iterator-seq (.find (.getRawGraph graph)))))

;; Check
(println "Arachne knowledge graph created with" (count-triples graph) "triples")

(defn get-labels
  [graph]
  (q/run graph '[?s ?o]
       '[:bgp [?s :rdfs/label ?o]]))

(defn describe-uri
  [uri]
  (reg/with {'g "http://syn.org/"}
            (q/run graph '[?p ?o]
                   `[:bgp [~uri ?p ?o]])))

(defn get-same-property
  "Get matching property property based on potentially same CDE"
  [uri]
  (reg/with {'g "http://syn.org/"}
            (q/run graph '[?match ?match_label]
                   `[:bgp [~uri :g/isCDE ?cde]
                     [?match :g/isCDE ?cde]
                     [?match :rdfs/label ?match_label]])))

(defn get-same-property-with-label 
  [label]
  (reg/with {'g "http://syn.org/"}
            (q/run graph '[?match ?match_label] 
            '[:bgp [?s :rdfs/label ?label] 
              [?s :g/isCDE ?cde]
              [?match :g/isCDE ?cde]
              [?match :rdfs/label ?match_label]] 
            `{?label ~label})))


(defn describe-template-columns
  "Describe columns in template in order"
  [template]
  (let [result
        (reg/with {'g "http://syn.org/"}
              (q/run graph '[?position ?column]
                `[:bgp
                  [?s :rdf/type :g/ColumnPosition]
                  [?s :g/template ~template]
                  [?s :g/column ?column]
                  [?s :g/position ?position]
                ]
                ))]
    (sort-by first result)
    ))

(defn get-col-position
  "Get position for an attribute within a specific template"
  [attribute template]
  (reg/with {'g "http://syn.org/"}
    (q/run graph '[?position]
      `[:bgp
        [?s :rdf/type :g/ColumnPosition]
        [?s :g/column ~attribute]
        [?s :g/template ~template]
        [?s :g/position ?position]
        ]
        )))

(defn list-templates
  "List associated templates given what should be a URI for the data standard."
  [uri]
  (reg/with {'g "http://syn.org/"
             'dct "http://purl.org/dc/terms/"}
            (q/run graph '[?template]
              `[:bgp
                [?template :rdf/type :g/Template]
                [?template :dct/conformsTo ~uri]])))

;; TESTS
;;
; (def standard "<http://syn.org/gdc>")
; (def a "<http://syn.org/gdc/study/study_name>")
; (def t "<http://syn.org/gdc/study>")
; (get-same-property-with-label "dbgap_accession")
