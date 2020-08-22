(defmodule dnet-main-handler
  (export all)
  (import (from exemplar-html (html 1))))

(defun content_types_provided (req state)
  (io:format "content_types_provided ~p~n" `(,req))
  `#([#(#"application/json" handle-get)] ,req ,state))

(defun handle-get (req state)
  (io:format "handle-get ~p~n" `(,req))
  (let ((body #"{\"status\": \"ok\"}"))
    `#(,body ,req ,state)))

(defun init (req opts) 
  `#(cowboy_rest ,req ,opts))

(defun terminate (_req _state)
 'ok)
