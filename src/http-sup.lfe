#|
@doc
  http supervisor
@end
|#

(defmodule http-sup
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
  'http-sup)

(defun event-manager-name ()
  'event-man)

(defun start_link ()
  (let ((`#(ok ,pid) (supervisor:start_link `#(local ,(server-name))
                                            (MODULE) '())))
    `#(ok ,pid)))

;; --- Supervisor callbacks ----------------------------------------------------

(defun init (_args)
  (let ((sup-flag #M(strategy simple_one_for_one
                     intensity 1
                     period 5))
        (child-spec `(#(http #(http start_link ()) transient 2000 worker
                             [http]))))
    `#(ok #(,sup-flag ,child-spec))))

;; --- API functions ------------------------------------------------------

(defun start-child (uri-map)
  (supervisor:start_child (server-name) uri-map))

(defun terminate-child (pid)
  (supervisor:terminate_child (server-name) pid))
