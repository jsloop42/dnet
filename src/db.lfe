(defmodule db
  (behaviour gen_server)
  ;; gen_server implementation
  (export
   (start_link 1)
   (start 0)
   (stop 0))

  ;; callback implementation
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3))

  ;; server API
  (export
   (hello 0)
   (init-deps 0)
   (setup-mnesia 0)
   (create-table 0)
   (delete-table 1)))

(include-lib "include/table.lfe")

;; --- config functions --------------------------------------------------------

(defun server-name () (MODULE))

(defun callback-module () (MODULE))

(defun initial-state () 0)

(defun genserver-opts () '())

; (defun register-name () `#(local ,(server-name)))

(defun unknown-command () #(error "Unknown command."))

(defun http-get-state-map () #m(chunk 0))

;; --- gen_server --------------------------------------------------------------

(defun start_link (url-info)
  (gen_server:start_link (callback-module)
                         url-info
                         (genserver-opts)))

(defun start ()
  (gen_server:start (callback-module)
                    (initial-state)
                    (genserver-opts)))

(defun stop ()
  (gen_server:call (server-name) 'stop))

;; --- callbacks ---------------------------------------------------------------

(defun init (state-data)
  (process_flag 'trap_exit 'true)
  `#(ok ,state-data))

(defun handle_cast
  (('get state-data)
   (io:format "http cast get state: ~p~n" `())
   `#(noreply url-info))
  (('post state-data)
   `#(noreply state-data)))

(defun handle_call
  (('get _caller state-data)
   `#(reply ,(tuple 'ok #"success") state-data))
  (('stop _caller state-data)
   `#(stop shutdown ok state-data))
  ((message _caller state-data)
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

(defun init-deps ()
  (mnesia:start))

(defun setup-mnesia ()
  (case (mnesia:create_schema `(,(node)))  ; This needs to be done only once for a node.
    ('ok 'ok)
    (`#(error ,any) (progn
                      (io:format "Mnesia setup error ~p~n" `(,any))
                      'ok))))

(defun create-table ()
  ;; create table for user
  (mnesia:create_table 'file-type `(#(attributes ,(fields-file-type))
                                    #(type set)
                                    #(disc_copies (,(node)))))
  (mnesia:create_table 'subscription `(#(attributes ,(fields-subscription))
                                       #(type set)
                                       #(disc_copies (,(node)))))
  (mnesia:create_table 'user `(#(attributes ,(fields-user)) #(type set)))
  (mnesia:create_table 'table-meta `(#(attributes ,(fields-table-meta))
                                     #(type set)
                                     #(disc_copies (,(node))))))

(defun delete-table (names)
  (lc ((<- x names))
    (mnesia:delete_table x)))
