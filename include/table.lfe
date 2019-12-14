;; Child resources that a resource points to, mainly in case of html files.
;; (defrecord resource-link
;;  id
;;  resource-id
;;  child-resource-id
;;  status)

;; Holds all file types supported.
(defrecord file-type
  mime-type
  name
  extension
  date-created
  date-modified
  status
  version)

(defrecord subscription
  subscription-id
  name
  date-created
  date-modified
  version
  status)

;; Holds user information.
(defrecord user
  user-id
  first-name
  last-name
  email
  username
  date-created
  date-modified
  version
  is-email-verified
  subscription)

;; A table holding the last id for each table that exists.
(defrecord table-meta
  table-name
  last-id)

