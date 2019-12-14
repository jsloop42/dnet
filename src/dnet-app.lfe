#|
@doc
  dnet public API
@end
|#

(defmodule dnet-app
  (behaviour application)

  ;; Application callbacks
  (export (start 2)
          (stop 1)))

;; API

(defun start (_start-type _start-args)
  "OTP main."
  (let* ((dispatch (cowboy_router:compile (router-match-spec)))  ; init cowboy, routes
         (`#(ok ,_any)
          (cowboy:start_clear
           'http-listener 
           '(#(port 8080)) 
           (map 'env (map 'dispatch dispatch))))
         (`#(ok ,sup-pid)
          (dnet-sup:start_link)))
    `#(ok ,sup-pid)))

(defun stop (_state)
  (cowboy:stop_listener 'http-listener)
  'ok)

(defun router-match-spec ()
  "Cowboy routes."
  '(#(_ (#("/" dnet-main-handler ())
         #("/http" dnet-http-handler ())))))
