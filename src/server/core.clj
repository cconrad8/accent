(ns server.core
  (:gen-class)
  (:require [accent.state :refer [setup u]]
            [accent.chat :refer [stream-response save-messages]]
            [agents.syndi :refer [OpenAISyndiAgent]]
            [org.httpkit.server :as httpkit]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]
            [babashka.http-client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]
            [ring.util.response :as response]
            [clojure.java.io :as io]
            [database.dlvn :refer [run-query conn unique-dccs]]
            [hiccup.core :refer [html]]
            [clojure.java.browse :refer [browse-url]]
            [com.brunobonacci.mulog :as mu]
            ;; MCP
            [server.mcp.core :as mcp]
            [server.mcp.transport :as transport]
            [server.mcp.schema :as schema]
            [server.mcp.utils :as utils]))

(mu/set-global-context! {:app-name "accent-server"})
(mu/start-publisher! {:type :console})

(def clients (atom #{}))

(def dcc-list-tool
  (schema/create-tool
    "list-dccs"
    "List all available DCCs"
    {}
    (fn [_] "No DCCs found")))

(def mcp-server 
  (delay
    (let [server (mcp/create-server
                  {:name "Accent MCP Server"
                   :version "1.0.0"
                   :initialize-fn (fn [_]
                                    (mu/log ::mcp-server-initialized))
                   :shutdown-fn (fn []
                                  (mu/log ::mcp-server-shutdown))
                   :on-receive (fn [msg]
                                (mu/trace ::mcp-message-received))
                   :on-send (fn [msg]
                             (mu/trace ::mcp-message-sent))})]
      (-> server
          (mcp/register-tool! dcc-list-tool))
      
      (mu/log ::mcp-server-created)
      server)))

(defn options-modal-html []
  (let [dccs ["A"]]; (mapv first (run-query @conn unique-dccs))]
    (html
     [:div#options-modal.modal
      [:div.modal-content
       [:h2 "Select your DCC"]
       [:select#dcc-select
        [:option {:value ""} "Choose a DCC"]
        (for [dcc dccs]
          [:option {:value dcc} dcc])]
       [:button#dcc-submit "Set DCC"]]])))

(defn handle-message [msg channel]
  (let [parsed-msg (json/parse-string msg true)]
    (case (:type parsed-msg)
      "set_dcc"
      (do
        (swap! u assoc :dcc (:dcc parsed-msg)))
        ;; (httpkit/send! channel (json/generate-string {:type "dcc-set" :dcc (:dcc parsed-msg)})))

      "chat"
      (future (stream-response OpenAISyndiAgent (:content parsed-msg) nil clients))

      "save"
      (let [saved-messages (save-messages OpenAISyndiAgent)] 
        (doseq [client @clients]
          (httpkit/send! client (json/generate-string {:type "system-message" :message (str "Saved as " saved-messages ".")}))))

      "stop"
      (do
        (doseq [client @clients]
          (httpkit/send! client (json/generate-string {:type "system-message" :message (str "Stopping app...")})))
        (System/exit 0))

      (httpkit/send! channel (json/generate-string {:type "error" :message "Unknown message type"})))))

(defn ws-handler [req]
  (httpkit/with-channel req channel
    (httpkit/send! channel (json/generate-string {:type "system-message" :message "Connected to server."}))
    (httpkit/on-receive channel (fn [msg] (handle-message msg clients)))
    (httpkit/on-close channel (fn [status] (swap! clients disj channel)))
    ;; Add client to the set
    (swap! clients conj channel)))

(defn mcp-ws-handler [req]
  (let [handler (transport/websocket-handler @mcp-server)]
    (handler req)))

;; Routes based on configuration
(defn create-routes [mode]
  (condp = mode
    :mcp-server 
    (defroutes mcp-server-routes
      (GET "/" [] (response/resource-response "index.html" {:root "public"}))
      (GET "/mcp" [] mcp-ws-handler)
      (route/resources "/")
      (route/not-found "Not Found"))
    
    :legacy
    (defroutes legacy-routes
      (GET "/" [] (response/resource-response "index.html" {:root "public"}))
      (GET "/ws" [] ws-handler)
      (GET "/options-modal" [] (response/response (options-modal-html)))
      (route/resources "/")
      (route/not-found "Not Found"))
    
    :full
    (defroutes full-routes
      (GET "/" [] (response/resource-response "index.html" {:root "public"}))
      (GET "/ws" [] ws-handler)
      (GET "/mcp" [] mcp-ws-handler)
      (GET "/options-modal" [] (response/response (options-modal-html)))
      (route/resources "/")
      (route/not-found "Not Found"))))

;; Server startup function
(defn start-server
  ([] (start-server (:mode config)))
  ([mode]
   (mu/log ::server-starting
          :port (:port config)
          :mode mode)
   
   ;; Setup based on mode
   (when (#{:full :legacy} mode)
     (setup :ui :web)
     (swap! u assoc :stream true))
   
   ;; Force initialization of MCP server if needed
   (when (#{:full :mcp-server} mode)
     @mcp-server)
   
   ;; Start HTTP server with appropriate routes
   (let [routes (create-routes mode)
         server (httpkit/run-server routes {:port (:port config)})]
     
     (mu/log ::server-started
            :port (:port config)
            :mode mode)
     
     (when (:open-browser config)
       (browse-url (str "http://localhost:" (:port config))))
     
     server)))

(defn -main [& args]
  (let [mode-arg (first args)
        mode (case mode-arg
               "mcp-server" :mcp-server
               "legacy" :legacy
               :full)]
    (start-server mode)))
