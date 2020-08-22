#|
@doc
  dnet top level supervisor
@end
|#

(defmodule dnet-sup
  (behaviour supervisor)

  ;; API
  (export (start_link 0))
  ;; Supervisor callbacks
  (export (init 1)))

;; --- API functions -----------------------------------------------------------

(defun server-name ()
  'dnet-sup)

(defun start_link ()
  (let ((`#(ok ,pid) (supervisor:start_link `#(local ,(server-name))
                                            (MODULE) '())))
    `#(ok ,pid)))

;; --- Supervisor callbacks ----------------------------------------------------

(defun init (_args)
  (let ((sup-flag #M(strategy one_for_one
                     intensity 1
                     period 5))
        (child-spec `(#(dnet-svc-sup
                        #(dnet-svc-sup start_link ())
                        permanent 2000 supervisor [dnet-svc-sup])
                      #(db-sup
                        #(db-sup start_link ())
                        permanent 2000 supervisor [db-sup])
                      #(http-sup #(http-sup start_link ()) permanent 2000
                                 supervisor [http-sup]))))
    `#(ok #(,sup-flag ,child-spec))))

;; --- Internal functions ------------------------------------------------------
