(ns server.mcp.utils
  "Utility functions for MCP server implementation"
  (:require [com.brunobonacci.mulog :as mu]))

;; Duration calculation helper
(defn duration-since [start-time]
  (when start-time
    (let [now (java.time.Instant/now)
          duration (java.time.Duration/between start-time now)]
      (.toMillis duration))))

;; Tool schema conversion
(defn tool->schema
  "Convert a tool record to its schema representation"
  [tool]
  {:name (:name tool)
   :description (:description tool)
   :parameters (into {}
                     (for [[param-name param-def] (:parameters tool)]
                       [param-name
                        {:type (-> param-def :type :schema-type)
                         :description (:description param-def)
                         :required (boolean (:required param-def))}]))})

;; Resource schema conversion
(defn resource->schema
  "Convert a resource record to its schema representation"
  [resource]
  {:id (:id resource)
   :description (:description resource)
   :parameters (into {}
                     (for [[param-name param-def] (:parameters resource)]
                       [param-name
                        {:type (-> param-def :type :schema-type)
                         :description (:description param-def)
                         :required (boolean (:required param-def))}]))})

;; Prompt schema conversion
(defn prompt->schema
  "Convert a prompt record to its schema representation"
  [prompt]
  {:id (:id prompt)
   :description (:description prompt)
   :parameters (into {}
                     (for [[param-name param-def] (:parameters prompt)]
                       [param-name
                        {:type (-> param-def :type :schema-type)
                         :description (:description param-def)
                         :required (boolean (:required param-def))}]))})

;; Parameter validation
(defn validate-parameter
  "Validate a parameter value against its definition"
  [param-def value]
  (when-let [validator (-> param-def :type :validate)]
    (validator value)))

;; Tool argument validation
(defn validate-tool-args
  "Validate all arguments for a tool"
  [tool args]
  (doseq [[param-name param-def] (:parameters tool)]
    (when (and (:required param-def) 
               (not (contains? args param-name)))
      (throw (ex-info (str "Missing required parameter: " param-name)
                      {:type :validation-error
                       :parameter param-name})))
    
    (when-let [value (get args param-name)]
      (when-not (validate-parameter param-def value)
        (throw (ex-info (str "Invalid parameter type: " param-name)
                        {:type :validation-error
                         :parameter param-name
                         :expected (-> param-def :type :schema-type)
                         :actual (type value)}))))))

(defn validate-resource-params [resource params]
  (doseq [[param-name param-def] (:parameters resource)]
    (when (and (:required param-def) 
               (not (contains? params param-name)))
      (throw (ex-info (str "Missing required parameter: " param-name)
                      {:type :validation-error
                       :parameter param-name})))
    
    (when-let [value (get params param-name)]
      (when-not (validate-parameter param-def value)
        (throw (ex-info (str "Invalid parameter type: " param-name)
                        {:type :validation-error
                         :parameter param-name
                         :expected (-> param-def :type :schema-type)
                         :actual (type value)}))))))

(defn validate-prompt-params [prompt params]
  (doseq [[param-name param-def] (:parameters prompt)]
    (when (and (:required param-def) 
               (not (contains? params param-name)))
      (throw (ex-info (str "Missing required parameter: " param-name)
                      {:type :validation-error
                       :parameter param-name})))
    
    (when-let [value (get params param-name)]
      (when-not (validate-parameter param-def value)
        (throw (ex-info (str "Invalid parameter type: " param-name)
                        {:type :validation-error
                         :parameter param-name
                         :expected (-> param-def :type :schema-type)
                         :actual (type value)}))))))

;; Tool invocation
(defn invoke-tool
  "Invoke a tool with arguments and return results"
  [tool args]
  (try
    (let [result ((:handler tool) args)]
      {:success true
       :results (if (sequential? result) result [result])})
    (catch Exception e
      (mu/log ::tool-invocation-error
             :tool-name (:name tool)
             :error-message (.getMessage e)
             :exception-data (ex-data e))
      
      {:success false
       :errors [(str "Tool execution error: " (.getMessage e))]})))

;; Resource retrieval
(defn get-resource-content
  "Get content of a resource with parameters"
  [resource params]
  (try
    (let [content ((:handler resource) params)]
      {:success true
       :content content})
    (catch Exception e
      (mu/log ::resource-retrieval-error
             :resource-id (:id resource)
             :error-message (.getMessage e)
             :exception-data (ex-data e))
      
      {:success false
       :errors [(str "Resource retrieval error: " (.getMessage e))]})))

;; Prompt processing
(defn process-prompt
  "Process a prompt with parameters"
  [prompt params]
  (try
    (let [content ((:handler prompt) params)]
      {:success true
       :content content})
    (catch Exception e
      (mu/log ::prompt-processing-error
             :prompt-id (:id prompt)
             :error-message (.getMessage e)
             :exception-data (ex-data e))
      
      {:success false
       :errors [(str "Prompt processing error: " (.getMessage e))]})))
