(defmodule dnet
  (behaviour gen_server)
  ;; gen_server
  (export (start_link 1) (start 0) (stop 0))
  ;; callbacks
  (export (init 1) (handle_call 3) (handle_cast 2) (handle_info 2)
          (terminate 2) (code_change 3)))

;; --- config functions --------------------------------------------------------

(defun server-name () (MODULE))

(defun callback-module () (MODULE))

(defun initial-state () 0)

(defun genserver-opts () '())

;(defun register-name () `#(local ,(server-name)))

(defun unknown-command () #(error "Unknown command."))

;; --- gen_server --------------------------------------------------------------

(defun start_link (state)
  (gen_server:start_link (callback-module)
                         state
                         (genserver-opts)))

(defun start ()
  ;(db:setup-mnesia)
  (db:init-deps)
  (application:ensure_all_started 'dnet))

(defun stop ()
  (gen_server:call (server-name) 'stop))

;; --- callbacks ---------------------------------------------------------------

(defun init (url-map)
  (process_flag 'trap_exit 'true)
  (let* ((uri-map (utils:parse-uri (map-get url-map 'url)))
         (`#(ok ,http-pid) (http-sup:start-child `(,uri-map)))
         (state `#m(uri-map ,uri-map http-pid ,http-pid)))
    (gen_server:cast http-pid 'get)
   `#(ok ,state)))

(defun handle_cast
  (('get-page state-data)
   (io:format "dnet:handle_cast 'get-page state: ~p~n" `(,state-data))
   `#(noreply state-data))
  (('post state-data)
   `#(noreply state-data)))

(defun handle_call
  (((tuple 'get-page url) _caller state-data)
   (io:format "dnet:handle_call 'get-page~n")
   `#(reply ,(tuple 'ok #"success") state-data))
  (('stop _caller state-data)
   `#(stop normal shutdown_ok state-data))
  ((message _caller state-data)
   (io:format "dnet:handle_call unknown message ~p~n" `(,message))
   `#(reply ,(unknown-command) ,state-data)))

(defun handle_info
  "Handles undefined messages."
  ((`#(EXIT ,pid normal) state-data)
   (io:format "Process ~p exited normally~n" `(,pid))
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (io:format "Process ~p exited (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state-data))
  ((_msg state-data)
   `#(noreply ,state-data)))

(defun terminate (_reason _state-data)
  (io:format "~p terminate ~p~n" `(,(server-name) ,_reason))
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;; --- server API --------------------------------------------------------------
