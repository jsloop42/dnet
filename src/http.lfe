(defmodule http
  (behaviour gen_server)
  (export
   ;; gen_server implementation
   (start_link 1)
   (start 0)
   (stop 0)
   ;; callback implementation
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3)
   ;; server API
   (get 1)))

;; --- config functions --------------------------------------------------------

(defun server-name () (MODULE))

(defun callback-module () (MODULE))

(defun initial-state () 0)

(defun genserver-opts () '())

(defun register-name () `#(local ,(server-name)))

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

(defun init (url-info)
  (process_flag 'trap_exit 'true)
  `#(ok ,url-info))

(defun handle_cast
  (('get url-info)
   (io:format "http cast get state: ~p~n" `(,url-info))
   (get url-info)
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

(defun get (url-map)
  (io:format "http get ~p~n" `(,url-map))
  (let* ((`#(ok ,con-pid) (gun:open (binary:bin_to_list (map-get url-map 'host))
                                    (if (== (map-get url-map 'scheme) #"https")
                                      443
                                      80)))
         (mon-ref (monitor 'process con-pid)))
    (receive
      (msg
       (progn
         (io:format "gun:open msg ~p~n" `(,msg))
         (handle-get-request url-map con-pid mon-ref (http-get-state-map))))
     ((after 3000) (exit 'timeout)))))

(defun handle-get-request (url-map con-pid mon-ref state)
  (io:format "handle get request path: ~p~n" `(,(map-get url-map 'path))) 
  (let ((stream-ref (gun:get con-pid (map-get url-map 'path))))
    (receive
      ((tuple 'gun_response con-pid stream-ref 'fin status headers)
       (progn
         (io:format "get request fin~nstatus ~p~nheaders ~p~n"
                    `(,status ,headers))
         'no_data))
      ((tuple 'gun_response con-pid stream-ref 'nofin status headers)
       (progn
         (io:format "get request body nofin status ~p~nheaders: ~p~n"
                    `(,status ,headers))
         (handle-get-response url-map con-pid mon-ref stream-ref state)))
      ((tuple 'DOWN mon-ref 'process con-pid reason)
       (io:format "handle get request error ~p~n" `(,reason))
       (exit reason))
      ((after 5000) (exit 'timeout)))))

;; (defun await (con-pid stream-ref)
;;   (io:format "await method~n")
;;   (case (gun:await con-pid stream-ref)
;;     ((tuple 'response 'fin status headers)
;;      'nodata)
;;     ((tuple 'response 'nofin status headers)
;;      (let ((`#(ok ,body) (gun:await_body con-pid stream-ref)))
;;        (io:format "await ~s~n" (list body))))))

(defun handle-get-response (url-map con-pid mon-ref stream-ref state)
  (receive
    ((tuple 'gun_data con-pid stream-ref 'nofin data)
     (progn
       (let* ((chunk (map-get state 'chunk))
              (state* (map-update state 'chunk (+ chunk 1))))
         ;(if (>= chunk 2)
         ;  (progn
         ;    (io:format "Cancelling gun stream ~p~n" `(,stream-ref))
         ;    (gun:cancel con-pid stream-ref))
         (handle-get-response url-map con-pid mon-ref stream-ref state*))))
    ((tuple 'gun_data con-pid stream-ref 'fin data)
     (io:format "handle-get response finished ~p~n" `(,data)))
    ((tuple 'DOWN mon-ref 'process con-pid reason)
     (io:format "handle-get request error ~p~n" `(,reason))
     (exit reason))
    ((after 5000) (exit 'timeout))))
