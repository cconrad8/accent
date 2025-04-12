(ns server.mcp.schema
  "Schema definitions, error codes, and types for MCP"
  (:require [com.brunobonacci.mulog :as μ]))

;; MCP Protocol version
(def latest-protocol-version "2025-03-26")

;; JSON-RPC error codes
(def error-parse -32700)
(def error-invalid-request -32600)
(def error-method-not-found -32601)
(def error-invalid-params -32602)
(def error-internal -32603)

;; MCP error codes (-32000 to -32099 range)
(def error-initialize-failed -32000)
(def error-request-cancelled -32001)

;; Parameter types
(def string-type
  {:validate string?
   :schema-type "string"})

(def number-type
  {:validate number?
   :schema-type "number"})

(def boolean-type
  {:validate boolean?
   :schema-type "boolean"})

(def array-type
  {:validate sequential?
   :schema-type "array"})

(def object-type
  {:validate map?
   :schema-type "object"})

;; Tool record (could be in tools.clj, but here for reference)
(defrecord Tool [name description parameters handler])

;; Resource record
(defrecord Resource [id description parameters handler])

;; Prompt record
(defrecord Prompt [id description parameters handler])

;; Factory functions
(defn create-tool
  "Create a new tool with name, description, parameters and handler"
  [name description parameters handler]
  (->Tool name description parameters handler))

(defn create-resource
  "Create a new resource with id, description, parameters and handler"
  [id description parameters handler]
  (->Resource id description parameters handler))

(defn create-prompt
  "Create a new prompt with id, description, parameters and handler"
  [id description parameters handler]
  (->Prompt id description parameters handler))
