(ns agents.arachne
  (:gen-class)
  (:require [accent.state :refer [setup u]]
            [accent.chat :as chat]
            [database.arachne :as arachne]
            [cheshire.core :as json]
            [clojure.string :as str]
            [com.brunobonacci.mulog :as mu]))

;;;;;;;;;;;;;;;;;;;;;
;; Tool defs
;;;;;;;;;;;;;;;;;;;;;

(def find_matching_attribute_spec
  {:type "function"
   :function
   {:name "find_matching_attribute"
    :description (str "Given a source attribute, find a matching attribute in a target attribute set. "
                      "This will return the matching/pairing attribute if it exists. "
                      ;;"If matching attribute found, information like attribute description, etc. is also returned."
                      )
    :parameters
    {:type "object"
     :properties
     {:attribute_uri
      {:type "string"
       :description "The source attribute URI, should be in the format '<http://syn.org/{data_standard}/{template}/{attribute}>', e.g. '<http://syn.org/ccdi/sample/sample_id>'."}
     }
    }
    :required ["attribute_uri"] }})

(def get_attribute_meta_spec
  {:type "function"
   :function
   {:name "get_attribute_meta"
    :description (str "Get all information for an attribute using its URI.")
    :parameters
    {:type "object"
     :properties
     {:attribute_uri
      {:type "string"
       :description "The attribute URI, generally of the format '<http://syn.org/{data_standard}/{template}>', e.g. '<http://syn.org/gdc/sample>'."}}}
    :required ["attribute_uri"]}})

(def get_template_meta_spec
  {:type "function"
   :function
   {:name "get_template_meta"
    :description (str "Get information about an entity template such as its attributes (columns) and their order.")
    :parameters
    {:type "object"
     :properties
     {:template_uri
      {:type "string"
       :description "The template URI, generally of the format '<http://syn.org/{data_standard}/{template}>', e.g. '<http://syn.org/gdc/sample>'."}}}
    :required ["template_uri"]}})

(def list_standard_templates_spec
  {:type "function"
   :function
   {:name "list_standard_templates"
    :description (str "List the templates defined by a data standard. The template URIs are returned and 'get_template_meta' can be used to get further info about a specific template.")
    :parameters
    {:type "object"
     :properties
     {:standard_uri
      {:type "string"
       :enum ["<http://syn.org/gdc>"]
       :description "The data standard URI, of the format '<http://syn.org/{data_standard}>', i.e. '<http://syn.org/gdc>'. Currently, only GDC standard is supported."}}}
    :required ["standard_uri"]}})

(def read_csv_spec
  {:type "function"
   :function
   {:name "read_csv"
    :description "Read data from a CSV file accessible as a local file or via a URL. Excel files are *not* supported."
    :parameters
    {:type "object"
     :properties
     {:file {:type "string" 
             :description "Local file path such as 'input/sample.csv' or URL such as 'https://raw.githubusercontent.com/codeforamerica/ohana-api/refs/heads/master/data/sample-csv/organizations.csv'"}
      }}
    :required ["file"]}})

(def write_csv_spec
  {:type "function"
   :function
   {:name "write_csv"
    :description "Write data to csv file."
    :parameters
    {:type "object"
     :properties
     {:data
      {:type "string"
       :description "CSV data conforming to a standard template."}
      :filename
      {:type "string"
       :description "Name for the csv file to be written, including the .csv extension."}}}
    :required ["data" "filename"] }})


(def tools
  [find_matching_attribute_spec
   get_attribute_meta_spec
   get_template_meta_spec
   list_standard_templates_spec
   read_csv_spec
   write_csv_spec
   ])

(def anthropic-tools (chat/convert-tools-for-anthropic tools true))

;;;;;;;;;;;;;;;;;;;;;;
;; Tool call wrappers
;;;;;;;;;;;;;;;;;;;;;;

(defn wrap-find-matching-attribute
  [{:keys [attribute_uri]}]
  (let [result (arachne/get-same-property attribute_uri)]
    (if (empty? result)
      {:result "No known matches were found."
       :type :success}
      {:result (str result)
       :type :success})))

(defn wrap-get-attribute-meta 
  [{:keys [attribute_uri]}]
  (let [result (arachne/describe-uri attribute_uri)]
    {:result (str result)
     :type :success}))

(defn wrap-get-template-meta
  [{:keys [template_uri]}]
  (let [result (arachne/describe-template-columns template_uri)]
    {:result (str result)
     :type :success}))

(defn wrap-list-standard-templates
  [{:keys [standard_uri]}]
  (let [result (arachne/list-templates standard_uri)]
    {:result (str result)
     :type :success}))

(defn wrap-read-csv 
  [{:keys [file]}]
  (let [is-url? (re-find #"^https?://" file)
        retry-limit 3]

    (letfn [(attempt-read [attempt]
              (try
                (if is-url?
                  ;; Remote file (URL)
                  (let [content (slurp file)]
                    {:result content :type :success})
                  ;; Local file
                  (let [f (clojure.java.io/file file)]
                    (if (.exists f)
                      {:result (slurp f) :type :success}
                      {:result (str "Error: Local file not found - " file)
                       :type :error
                       :error true})))
                (catch java.io.FileNotFoundException e
                  {:result (str "Error: File not found - " (.getMessage e))
                   :type :error
                   :error true})
                (catch java.net.UnknownHostException e
                  {:result (str "Error: Cannot reach host - " (.getMessage e))
                   :type :error
                   :error true})
                (catch java.io.IOException e
                  (let [msg (.getMessage e)]
                    (if (and is-url? (< attempt retry-limit)
                             (or (re-find #"529" msg)
                                 (re-find #"429" msg)
                                 (re-find #"Too many requests" msg)))
                      (do
                        (println (str "HTTP throttled (attempt " (inc attempt) "): " msg))
                        (Thread/sleep (* 1000 (Math/pow 2 attempt))) ;; exponential backoff
                        (attempt-read (inc attempt)))
                      {:result (str "Error reading CSV file: " msg)
                       :type :error
                       :error true})))
                (catch Exception e
                  {:result (str "Error reading CSV file: " (.getMessage e))
                   :type :error
                   :error true})))]

      (attempt-read 0))))

(defn wrap-write-csv 
  [{:keys [data filename]}]
  (let [result (spit filename data)]
    {:result "File written successfully."
     :type :success}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom tool time
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn with-next-tool-call
  "Applies logic for chaining certain tool calls. Input should be result from `tool-time`
  Currently, stage_curated should be forced after curate_dataset only under certain return types."
  [tool-result]
  (if (and (= "curate_dataset" (tool-result :tool)) (= :success (tool-result :type)))
    (assoc tool-result :next-tool-call "stage_curated")
    tool-result))

(defn tool-time 
  [tool-call]
  (let [call-fn (get-in tool-call [:function :name])
        args    (json/parse-string (get-in tool-call [:function :arguments]) true)]
    (try
      ;; Announce which function is being run
      (println (str "🔧 Running tool function: " call-fn))

      ;; Throttling delay with countdown
      (println "Throttling to avoid rate limits. Please wait...")
      (dotimes [i 60]
        (print (format "\rWaiting %2d seconds..." (- 60 i)))
        (flush)
        (Thread/sleep 1000))
      (println)

      ;; Execute the tool function
      (let [result (case call-fn
                     "find_matching_attribute"         (wrap-find-matching-attribute args)
                     "get_attribute_meta"              (wrap-get-attribute-meta args)
                     "get_template_meta"               (wrap-get-template-meta args)
                     "list_standard_templates"         (wrap-list-standard-templates args)
                     "read_csv"                        (wrap-read-csv args)
                     "write_csv"                       (wrap-write-csv args)
                     (throw (ex-info "Invalid tool function" {:tool call-fn})))]
        (-> (if (map? result)
              (merge {:tool call-fn} result)
              {:tool call-fn :result result})
            (with-next-tool-call)))
      (catch Exception e
        {:tool   call-fn
         :result (.getMessage e)
         :type   :error
         :error  true}))))

(defn anthropic-tool-time
  [tool-use]
  (let [tool-call {:id       (:id tool-use)
                   :type     "function"
                   :function {:name      (:name tool-use)
                              :arguments (json/generate-string (:input tool-use))}}]
    (tool-time tool-call)))

;;;;;;;;;;;;;;;;;;;;;
;; Agent
;;;;;;;;;;;;;;;;;;;;;

(def role
  (str 
  "You are a data management agent who specializes in reviewing diverse data templates based on the CCDI standard and translating them to a desired common standard called **GDC**. "
  "Your most common workflow consists of obtaining from the user the paths to one or more CSV templates of entity data that they need to transform to a target set of templates in the GDC standard. "
  "For each CSV file with records of some entity type, you can use the 'read_csv' tool to extract and see the entity data records. " 
  ;; "Then you can use the tool load_into_working_graph to put the entity data into an OLAP knowledge graph, which is similar to using a data warehouse for data transformations. " 
  "Given the input, you can query for information about matching target attributes and target templates to better understand specifications for the transformation. " 
  "For example, given a CSV file called 'sample.csv' that may contain column 'sample_attribute_1', you can see whether 'sample_attribute_1' matches an attribute in the GDC standard, which GDC template it's used in, and acceptable values for the GDC version of the attribute. "
  "You may retrieve the list of potential target templates in the GDC standard. Note that the inputs may not have a 1:1 match to the GDC templates, so not all GDC templates are output targets, only the relevant ones. "
  "Once you have determined which GDC templates to output and how to translate the data sufficiently, use the 'write_csv' tool. " 
  "You can adapt parts of the workflow as needed according to user needs and to get the best results, but in general seeing all csv file data first may be most optimal. "
   ))

(def openai-messages (atom [{:role "system" :content role}]))

(def anthropic-messages (atom []))

(def meta (atom {:system role}))

(def OpenAIArachneAgent 
  (chat/->OpenAIProvider "o3-mini" 
                   openai-messages
                   tools 
                   tool-time
                   meta))

(def AnthropicArachneAgent 
  (chat/->AnthropicProvider "claude-3-7-sonnet-latest" 
                      anthropic-messages
                      anthropic-tools 
                      anthropic-tool-time
                      meta))

(defn -main [] 
  (setup)

  (let [agent (if (= (@u :model-provider) "")
                OpenAIArachneAgent 
                AnthropicArachneAgent)]
    
    ;; Add shutdown hook to handle Ctrl+C
    (.addShutdownHook (Runtime/getRuntime)
      (Thread. (fn []
                 (try
                   ;; (chat/save-messages agent) ;; save based on config
                   (catch Exception e
                     (mu/log ::shutdown-error 
                             :msg "Error during shutdown" 
                             :exception e)))
                 (mu/log ::shutdown :msg "Goodbye!"))))
    
    (chat/chat agent)))
