(defmodule dnet-http-handler
  (export all)
  (import (from exemplar-html (html 1) (div 1))))

(defun allowed_methods (req state)
  `#([#"GET" #"POST"] ,req ,state))

(defun content_types_provided (req state)
  `#([#(#"text/html" handle-request)] ,req ,state))

(defun content_types_accepted (req state)
  `#([#(#"application/json" handle-request)] ,req ,state))

(defun handle-post (req state)
  (io:format "handle-post ~p~n" `(,req))
  (let* ((`#(ok ,body ,_) (cowboy_req:read_body req))
         ((tuple #"url" url) (ljson:decode body)))
    (io:format "handle-post ~p~n" `(,url))
    (dnet-svc-sup:start-child `(#m(url ,url)))
    `#(true ,(add-response-html-ok req state) ,state)))

(defun handle-request (req state)
  (io:format "handle-request ~p~nopts: ~p~n" `(,req ,state))
  (case (cowboy_req:method req)
    (#"POST" (handle-post req state))
    (#"GET" (let ((body #"{\"status\": \"get\"}"))
              `#(,body ,req ,state)))))

(defun init (req opts)
  `#(cowboy_rest ,req ,opts))

(defun terminate (_req _state)
  'ok)

;; --- json response functions -------------------------------------------------

(defun add-response-json-ok (req _state)
  (cowboy_req:set_resp_body (ljson:encode #(#"status" true)) req))

(defun get-response-json-ok ())


;; --- html response functions -------------------------------------------------

(defun add-response-html-ok (req _state)
  (let ((html-resp (div "Task scheduled successfully")))  
    (cowboy_req:set_resp_body html-resp req)))
