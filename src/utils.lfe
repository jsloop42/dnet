(defmodule utils
  (export all))

(defun parse-uri (uri)
  "Parse an HTTP URI into components."
  (uri_string:parse uri))
