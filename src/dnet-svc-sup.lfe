#|
@doc
  dnet service supervisor
@end
|#

(defmodule dnet-svc-sup
  (behaviour supervisor)
  ;; supervisor APIs
  (export (start_link 0))
  ;; Supervisor callbacks
  (export (init 1))
  ;; API functions
  (export (start-child 1)
          (terminate-child 1)))

;; --- config functions -----------------------------------------------------------

(defun server-name ()
  'dnet-svc-sup)

(defun start_link ()
  (let ((`#(ok ,pid) (supervisor:start_link `#(local ,(server-name))
                                            (MODULE) '())))
    `#(ok ,pid)))

;; --- Supervisor callbacks ----------------------------------------------------

(defun init (_args)
  (let ((sup-flag #M(strategy simple_one_for_one
                     intensity 1
                     period 5))
        (child-spec `(#(dnet #(dnet start_link ()) transient 2000 worker
                             [dnet]))))
    `#(ok #(,sup-flag ,child-spec))))

;; --- API functions ------------------------------------------------------

(defun start-child (xs)
  (io:format "dnet-svc-sup start child ~p~n" `(,xs))
  (supervisor:start_child (server-name) xs))

(defun terminate-child (pid)
  (supervisor:terminate_child (server-name) pid))

