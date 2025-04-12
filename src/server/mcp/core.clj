(ns server.mcp.core
  "Core MCP protocol definitions and server implementation"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [com.brunobonacci.mulog :as mu]
            [server.mcp.schema :as schema]
            [server.mcp.utils :as utils]))

;; Protocol version and capabilities
(def mcp-protocol-version "2025-03-26")

;; Protocol definition with clear documentation
(defprotocol MCPServer
  "Model Context Protocol (MCP) Server protocol.
   Defines the contract for all MCP server implementations."
  
  (initialize! [this client-info]
    "Initialize server with client capabilities and return server capabilities.
     This should be the first method called after connection.")
  
  (shutdown! [this]
    "Gracefully stop the server and clean up resources.")
  
  (list-tools [this]
    "Return a list of available tools with their schemas.")
  
  (list-resources [this]
    "Return a list of available resources with their schemas.")
  
  (list-prompts [this]
    "Return a list of available prompts with their schemas.")
  
  (call-tool [this tool-name args]
    "Execute a tool and return the results or errors.")
  
  (get-resource [this resource-id params]
    "Retrieve a resource with optional parameters.")
  
  (get-prompt [this prompt-id params]
    "Retrieve a prompt template with optional parameters.")
  
  (on-message-received [this message]
    "Hook called when a message is received from client.")
  
  (on-message-sent [this message]
    "Hook called when a message is sent to client.")
  
  (process-notification [this notification]
    "Process a notification from the client."))

;; Request ID counter for generating unique IDs
(def next-request-id (atom 0))

(defn generate-id []
  (swap! next-request-id inc))

;; Helper function to format tool results
(defn format-tool-results [results]
  {:content (mapv (fn [result]
                    {:type "text"
                     :text (str result)})
                  (if (sequential? results) results [results]))
   :isError false})

;; Helper function to format tool errors
(defn format-tool-errors [errors]
  {:content (mapv (fn [error]
                    {:type "text"
                     :text (str error)})
                  (if (sequential? errors) errors [errors]))
   :isError true})

;; Server implementation using defrecord
(defrecord StandardMCPServer [config tools-registry resources-registry prompts-registry state]
  MCPServer
  
  (initialize! [this client-info]
    (mu/log ::server-initialize
            :client-info client-info
            :server-name (:name config))
    
    (let [client-version (:version client-info)
          client-capabilities (:capabilities client-info)]
      
      (swap! state assoc 
             :initialized true
             :initialized-at (java.time.Instant/now)
             :client-version client-version
             :client-capabilities client-capabilities)
      
      ;; Run custom initialize function if provided
      (when-let [init-fn (:initialize-fn config)]
        (init-fn client-info))
      
      ;; Return server capabilities
      {:protocolVersion mcp-protocol-version
       :serverInfo {:name (:name config)
                    :version (:version config)}
       :capabilities {:tools {:listChanged (boolean (seq @tools-registry))}
                      :resources {:listChanged (boolean (seq @resources-registry))}
                      :prompts {:listChanged (boolean (seq @prompts-registry))}}}))
  
  (shutdown! [this]
    (mu/log ::server-shutdown
           :server-name (:name config)
           :uptime (utils/duration-since (:initialized-at @state)))
    
    (swap! state assoc :initialized false)
    
    (when-let [shutdown-fn (:shutdown-fn config)]
      (shutdown-fn))
    
    true)
  
  (list-tools [this]
    (mu/log ::list-tools
           :server-name (:name config)
           :count (count @tools-registry))
    
    (let [tools-list (mapv (fn [[tool-name tool]]
                             (assoc (utils/tool->schema tool)
                                    :name (name tool-name)))
                           @tools-registry)]
      tools-list))
  
  (list-resources [this]
    (mu/log ::list-resources
           :server-name (:name config)
           :count (count @resources-registry))
    
    (let [resources-list (mapv (fn [[resource-id resource]]
                                 (assoc (utils/resource->schema resource)
                                        :id (name resource-id)))
                               @resources-registry)]
      resources-list))
  
  (list-prompts [this]
    (mu/log ::list-prompts
           :server-name (:name config)
           :count (count @prompts-registry))
    
    (let [prompts-list (mapv (fn [[prompt-id prompt]]
                               (assoc (utils/prompt->schema prompt)
                                      :id (name prompt-id)))
                             @prompts-registry)]
      prompts-list))
  
  (call-tool [this tool-name args]
    (let [tool-key (keyword tool-name)
          tool (get @tools-registry tool-key)]
      
      (mu/log ::call-tool
             :server-name (:name config)
             :tool-name tool-name
             :args args
             :tool-exists (boolean tool))
      
      (if-not tool
        ;; Tool not found
        {:success false
         :errors [(str "Unknown tool: " tool-name)]}
        
        ;; Tool found, try to invoke it
        (try
          ;; First validate arguments
          (utils/validate-tool-args tool args)
          
          ;; Then invoke the tool
          (let [result (utils/invoke-tool tool args)]
            (mu/log ::tool-invoked
                   :tool-name tool-name
                   :success (:success result))
            result)
          
          (catch Exception e
            (mu/log ::tool-error
                   :tool-name tool-name
                   :error-message (.getMessage e)
                   :exception-data (ex-data e))
            
            {:success false
             :errors [(str "Error invoking tool: " (.getMessage e))]})))))
  
  (get-resource [this resource-id params]
    (let [resource-key (keyword resource-id)
          resource (get @resources-registry resource-key)]
      
      (mu/log ::get-resource
             :server-name (:name config)
             :resource-id resource-id
             :params params
             :resource-exists (boolean resource))
      
      (if-not resource
        ;; Resource not found
        {:success false
         :errors [(str "Unknown resource: " resource-id)]}
        
        ;; Resource found, try to retrieve it
        (try
          ;; Validate parameters
          (utils/validate-resource-params resource params)
          
          ;; Then get the resource
          (let [result (utils/get-resource-content resource params)]
            (mu/log ::resource-retrieved
                   :resource-id resource-id
                   :success (:success result))
            result)
          
          (catch Exception e
            (mu/log ::resource-error
                   :resource-id resource-id
                   :error-message (.getMessage e)
                   :exception-data (ex-data e))
            
            {:success false
             :errors [(str "Error retrieving resource: " (.getMessage e))]})))))
  
  (get-prompt [this prompt-id params]
    (let [prompt-key (keyword prompt-id)
          prompt (get @prompts-registry prompt-key)]
      
      (mu/log ::get-prompt
              :server-name (:name config)
              :prompt-id prompt-id
              :params params
              :prompt-exists (boolean prompt))
      
      (if-not prompt ;; Prompt not found
        {:success false
         :errors [(str "Unknown prompt: " prompt-id)]}
        
        ;; Prompt found, try to process it
        (try
          ;; Validate parameters
          (utils/validate-prompt-params prompt params)
          
          ;; Then process the prompt
          (let [result (utils/process-prompt prompt params)]
            (mu/log ::prompt-processed
                   :prompt-id prompt-id
                   :success (:success result))
            result)
          
          (catch Exception e
            (mu/log ::prompt-error
                   :prompt-id prompt-id
                   :error-message (.getMessage e)
                   :exception-data (ex-data e))
            
            {:success false
             :errors [(str "Error processing prompt: " (.getMessage e))]})))))
  
  (on-message-received [this message]
    (mu/trace ::message-received
             {:server-name (:name config)
              :message message})
    
    (when-let [receive-hook (:on-receive config)]
      (receive-hook message))
    
    message)
  
  (on-message-sent [this message]
    (mu/trace ::message-sent
             {:server-name (:name config)
              :message message})
    
    (when-let [send-hook (:on-send config)]
      (send-hook message))
    
    message)
  
  (process-notification [this notification]
    (mu/log ::notification-received
           :server-name (:name config)
           :method (:method notification))
    
    (when-let [notification-handler (:notification-handler config)]
      (notification-handler notification))
    
    nil))

;; Factory function for creating MCP servers
(defn create-server
  "Create a new MCP server with the given configuration, tools, resources, and prompts"
  [{:keys [name version initialize-fn shutdown-fn notification-handler on-receive on-send]
    :or {name "MCP Server"
         version "1.0.0"}}]
  
  (mu/log ::create-server
          :server-name name
          :version version)
  
  (let [config {:name name
                :version version
                :initialize-fn initialize-fn
                :shutdown-fn shutdown-fn
                :notification-handler notification-handler
                :on-receive on-receive
                :on-send on-send}
        
        tools-registry (atom {})
        resources-registry (atom {})
        prompts-registry (atom {})
        
        state (atom {:initialized false})]
    
    (->StandardMCPServer config tools-registry resources-registry prompts-registry state)))

;; Tool registration helper
(defn register-tool!
  "Register a tool with an MCP server"
  [server tool]
  (let [tool-name (keyword (:name tool))]
    (mu/log ::register-tool
            :server-name (-> server :config :name)
            :tool-name (:name tool))
    
    (swap! (:tools-registry server) assoc tool-name tool)
    server))

;; Resource registration helper
(defn register-resource!
  "Register a resource with an MCP server"
  [server resource]
  (let [resource-id (keyword (:id resource))]
    (mu/log ::register-resource
            :server-name (-> server :config :name)
            :resource-id (:id resource))
    
    (swap! (:resources-registry server) assoc resource-id resource)
    server))

;; Prompt registration helper
(defn register-prompt!
  "Register a prompt with an MCP server"
  [server prompt]
  (let [prompt-id (keyword (:id prompt))]
    (mu/log ::register-prompt
            :server-name (-> server :config :name)
            :prompt-id (:id prompt))
    
    (swap! (:prompts-registry server) assoc prompt-id prompt)
    server))
