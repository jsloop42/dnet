(defmodule utils
  (export all))

(defun parse-uri (uri)
  "Parses an HTTP URI into a components map."
  (uri_string:parse uri))
