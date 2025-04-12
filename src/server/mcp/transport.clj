(ns server.mcp.transport
  "Transport adapters for MCP server communication"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [com.brunobonacci.mulog :as mu]
            [server.mcp.core :as mcp]
            [server.mcp.schema :as schema]
            [server.mcp.utils :as utils]
            [org.httpkit.server :as httpkit]))

;; Transport protocol definition
(defprotocol Transport
  "Protocol for MCP transport implementations"
  (start! [this server]
    "Start the transport with the given server. Returns the transport.")
  
  (stop! [this]
    "Stop the transport. Returns true if successful.")
  
  (send! [this message]
    "Send a message through the transport. Returns true if successful.")
  
  (process-message [this server message]
    "Process an incoming message and return a response if needed."))

;; JSON-RPC helper functions
(defn parse-json-message
  "Parse a JSON message string into a Clojure data structure"
  [message-str]
  (try
    (json/parse-string message-str true)
    (catch Exception e
      (mu/log ::json-parse-error
              :error-message (.getMessage e)
              :message-str (when (< (count message-str) 1000) message-str))
      
      {:jsonrpc "2.0"
       :error {:code schema/error-parse
               :message "Parse error"}})))

(defn serialize-json-message
  "Serialize a Clojure data structure to a JSON string"
  [message]
  (try
    (json/generate-string message)
    (catch Exception e
      (mu/log ::json-serialize-error
             :error-message (.getMessage e)
             :message (pr-str message))
      
      (json/generate-string
        {:jsonrpc "2.0"
         :error {:code schema/error-internal
                 :message "Error serializing response"}}))))


(defn create-request
  "Create a JSON-RPC request message"
  [id method params]
  {:jsonrpc "2.0"
   :id id
   :method method
   :params params})

(defn create-notification
  "Create a JSON-RPC notification message (no response expected)"
  [method params]
  {:jsonrpc "2.0"
   :method method
   :params params})

(defn create-success-response
  "Create a successful JSON-RPC response"
  [id result]
  {:jsonrpc "2.0"
   :id id
   :result result})

(defn create-error-response
  "Create an error JSON-RPC response"
  [id code message & [data]]
  (cond-> {:jsonrpc "2.0"
           :id id
           :error {:code code
                   :message message}}
    data (assoc-in [:error :data] data)))

;; Message dispatch
(defn handle-jsonrpc-request
  "Handle a JSON-RPC request and return a response"
  [server {:keys [id method params] :as request}]
  (mu/trace ::handle-request
             :method method
             :id id)
  
  (try
    (case method
      "ping"
      (create-success-response id {})
      
      "initialize"
      (let [server-info (mcp/initialize! server params)]
        (create-success-response id server-info))
      
      "shutdown"
      (let [result (mcp/shutdown! server)]
        (create-success-response id result))
      
      "tools/list"
      (let [tools (mcp/list-tools server)]
        (create-success-response id {:tools tools}))
      
      "resources/list"
      (let [resources (mcp/list-resources server)]
        (create-success-response id {:resources resources}))
      
      "prompts/list"
      (let [prompts (mcp/list-prompts server)]
        (create-success-response id {:prompts prompts}))
      
      "tools/call"
      (let [{:keys [name arguments]} params
            result (mcp/call-tool server name arguments)]
        (if (:success result)
          (create-success-response id (utils/format-tool-results (:results result)))
          (create-success-response id (utils/format-tool-errors (:errors result)))))
      
      "resources/get"
      (let [{:keys [id parameters]} params
            result (mcp/get-resource server id parameters)]
        (if (:success result)
          (create-success-response id {:content (:content result)})
          (create-success-response id {:isError true
                                      :content (:errors result)})))
      
      "prompts/get"
      (let [{:keys [id parameters]} params
            result (mcp/get-prompt server id parameters)]
        (if (:success result)
          (create-success-response id {:content (:content result)})
          (create-success-response id {:isError true
                                      :content (:errors result)})))
      
      ;; Unknown method
      (do
        (mu/log ::unknown-method
               :method method)
        
        (create-error-response id 
                              schema/error-method-not-found
                              (str "Method not found: " method))))
    
    (catch Exception e
      (mu/log ::request-handler-error
             :method method
             :error-message (.getMessage e)
             :exception-data (ex-data e))
      
      (create-error-response id
                            schema/error-internal
                            (str "Internal error: " (.getMessage e))))))

;; JSON-RPC message handling
(defn handle-jsonrpc-notification
  "Handle a JSON-RPC notification (no response expected)"
  [server {:keys [method params] :as notification}]
  (mu/trace ::handle-notification
            :method method)

  (try
    (case method
      "initialized"
      (do
        (mu/log ::client-initialized)
        (mcp/process-notification server notification))

      ;; Other notifications
      (mcp/process-notification server notification))

    (catch Exception e
      (mu/log ::notification-handler-error
              :method method
              :error-message (.getMessage e)
              :exception-data (ex-data e))
      nil)))

(defn handle-jsonrpc-message
  "Handle a JSON-RPC message and return a response if needed"
  [server message]
  (mcp/on-message-received server message)

  (cond
    ;; Handle error messages
    (:error message)
    (do
      (mu/log ::received-error
              :error (:error message))
      nil)

    ;; Handle requests (have method & id)
    (and (:method message) (:id message))
    (let [response (handle-jsonrpc-request server message)]
      (mcp/on-message-sent server response)
      response)

    ;; Handle notifications (have method but no id)
    (and (:method message) (nil? (:id message)))
    (do
      (handle-jsonrpc-notification server message)
      nil)

    ;; Handle responses (have id and result/error)
    (and (:id message) (or (:result message) (:error message)))
    nil  ;; We don't process responses here

    ;; Unknown message type
    :else
    (do
      (mu/log ::unknown-message-type
              :message (pr-str message))
      nil)))

;; Batch handling
(defn is-batch-request?
  "Check if a message is a JSON-RPC batch request (array)"
  [message]
  (sequential? message))

(defn handle-batch-request
  "Process a batch of JSON-RPC requests and return a batch of responses"
  [server requests]
  (mu/log ::batch-request-received
         :count (count requests))
  
  (when (empty? requests)
    (mu/log ::empty-batch-request)
    (create-error-response nil 
                               schema/error-invalid-request
                               "Invalid batch request: empty array"))
  
  ;; Process each request in the batch and collect responses
  (let [responses (vec
                    (for [request requests
                          :let [response (handle-jsonrpc-message server request)]
                          :when response]  ;; Only include non-nil responses
                      response))]
    
    (mu/log ::batch-request-processed
           :response-count (count responses))
    
    (if (empty? responses)
      nil  ;; If no responses (all notifications), return nil
      responses)))

(defn process-message [transport server message-str]
  (mu/trace ::transport-receive
            :type (type transport)
            :message-str message-str)

  (let [message (parse-json-message message-str)]
    ;; Check if this is a batch request
    (if (is-batch-request? message)
      ;; Process as batch
      (let [batch-response (handle-batch-request server message)]
        (when batch-response
          (send! transport batch-response))
        batch-response)

      ;; Process as single request
      (let [response (handle-jsonrpc-message server message)]
        (when response
          (send! transport response))
        response))))

;; WebSocket transport implementation
(defrecord WebSocketTransport [options connections]
  Transport
  
  (start! [this server]
    (mu/log ::transport-start
            :type :websocket
            :options options)
    this)
  
  (stop! [this]
    (mu/log ::transport-stop
           :type :websocket)
    
    (doseq [conn @connections]
      (when (and conn (httpkit/open? conn))
        (httpkit/close conn)))
    
    (reset! connections #{})
    true)
  
 (send! [this message]
         (let [message-str (if (sequential? message)
                         ;; Handle batch response
                             (do
                               (μ/trace ::transport-send-batch
                                        :type :websocket
                                        :batch-size (count message))
                               (serialize-json-message message))
                         ;; Handle single response
                             (do
                               (μ/trace ::transport-send
                                        :type :websocket)
                               (serialize-json-message message)))]
 
           (doseq [conn @connections]
             (when (and conn (httpkit/open? conn))
               (httpkit/send! conn message-str))))
 
         true)
  
  (process-message [this server message-str]
    (mu/trace ::transport-receive
             :type :websocket
             :message-str message-str)
    
    (let [message (parse-json-message message-str)
          response (handle-jsonrpc-message server message)]
      
      (when response
        (send! this response))
      
      response)))

;; STDIO transport implementation
(defrecord StdioTransport [options reader writer]
  Transport
  
  (start! [this server]
    (mu/log ::transport-start
            :type :stdio
            :options options)
    
    (future
      (try
        (loop []
          (when-let [line (.readLine ^java.io.BufferedReader reader)]
            (when-not (str/blank? line)
              (mu/trace ::stdio-read
                        :line line)
              
              (let [message (parse-json-message line)
                    response (handle-jsonrpc-message server message)]
                
                (when response
                  (let [response-str (serialize-json-message response)]
                    (mu/trace ::stdio-write
                             :response response-str)
                    
                    (locking writer
                      (.write ^java.io.Writer writer (str response-str "\n"))
                      (.flush ^java.io.Writer writer)))))
              
              (recur))))
        
        (catch java.io.IOException e
          (mu/log ::stdio-io-error
                 :error-message (.getMessage e)))
        
        (catch Exception e
          (mu/log ::stdio-error
                 :error-message (.getMessage e)
                 :exception-data (ex-data e)))))
    
    this)
  
  (stop! [this]
    (mu/log ::transport-stop
           :type :stdio)
    true)
  
  (send! [this message]
    (let [message-str (serialize-json-message message)]
      (mu/trace ::transport-send
               :type :stdio
               :message-str message-str)
      
      (locking writer
        (.write ^java.io.Writer writer (str message-str "\n"))
        (.flush ^java.io.Writer writer)))
    
    true)
  
  (process-message [this server message-str]
    ;; This is handled by the read loop in start!
    nil))

;; HTTP transport implementation
(defrecord HttpTransport [options server-atom]
  Transport
  
  (start! [this server]
    (mu/log ::transport-start
           :type :http
           :options options)
    
    (let [port (or (:port options) 3000)
          
          app (fn [request]
                (case (:uri request)
                  "/mcp" 
                  (if (= (:request-method request) :post)
                    (let [body (slurp (:body request))
                          message (parse-json-message body)
                          response (handle-jsonrpc-message server message)]
                      
                      {:status 200
                       :headers {"Content-Type" "application/json"}
                       :body (when response
                               (serialize-json-message response))})
                    
                    {:status 405
                     :headers {"Content-Type" "application/json"}
                     :body (serialize-json-message
                             {:jsonrpc "2.0"
                              :error {:code -32000
                                      :message "Method not allowed"}})})
                  
                  ;; Not found
                  {:status 404
                   :headers {"Content-Type" "application/json"}
                   :body (serialize-json-message
                           {:jsonrpc "2.0"
                            :error {:code -32000
                                    :message "Not found"}})}))
          
          http-server (httpkit/run-server app {:port port})]
      
      (reset! server-atom http-server)
      
      (mu/log ::http-server-started
             :port port))
    
    this)
  
  (stop! [this]
    (mu/log ::transport-stop
            :type :http)
    
    (when-let [http-server @server-atom]
      (http-server :timeout 100)
      (reset! server-atom nil))
    
    true)
  
  (send! [this message]
    ;; HTTP transport is request/response based, so this is a no-op
    true)
  
  (process-message [this server message-str]
    ;; This is handled in the HTTP handler
    nil))

;; WebSocket handler for integration with existing server
(defn websocket-handler
  "HTTP Kit WebSocket handler for MCP"
  [server channel]
  (fn [request]
    (httpkit/with-channel request channel
      (let [transport (->WebSocketTransport {:id (str (random-uuid))} (atom #{channel}))]
        
        (mu/log ::websocket-connection
                :remote-addr (:remote-addr request))
        
        (httpkit/on-close channel 
                         (fn [status]
                           (mu/log ::websocket-closed
                                  :status status)))
        
        (httpkit/on-message channel 
                           (fn [message-str]
                             (process-message transport server message-str)))))))

;; Factory functions for creating transports
(defn websocket-transport
  "Create a WebSocket transport"
  [& [options]]
  (->WebSocketTransport (or options {}) (atom #{})))

(defn stdio-transport
  "Create an STDIO transport using the provided reader and writer or *in* and *out*"
  [& [{:keys [reader writer] :as options}]]
  (->StdioTransport 
    (or options {})
    (or reader *in*)
    (or writer *out*)))

(defn http-transport
  "Create an HTTP transport on the specified port"
  [& [options]]
  (->HttpTransport (or options {}) (atom nil)))

;; Start server with transport
(defn start-server!
  "Start an MCP server with the specified transport"
  [server transport]
  (mu/log ::start-server
          :server-name (-> server :config :name)
          :transport-type (type transport))
  
  (start! transport server)
  
  {:server server
   :transport transport})

;; Stop server
(defn stop-server!
  "Stop an MCP server and its transport"
  [{:keys [server transport]}]
  (mu/log ::stop-server
         :server-name (-> server :config :name))
  
  (stop! transport)
  (mcp/shutdown! server))
