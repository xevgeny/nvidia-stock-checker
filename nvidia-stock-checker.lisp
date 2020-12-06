(in-package #:nvidia-stock-checker)

; queries DE store (locale=de-de), founders editions only
(defvar *url* "https://api.nvidia.partners/edge/product/search?page=1&limit=9&locale=de-de&manufacturer=NVIDIA")
(defvar *show-alert?* nil)
(defvar *print-product-table?* t)
; checks availability in a loop
(defvar *loop?* nil)
; sleep interval in seconds, defaults to 5 minutes
(defvar *sleep-interval* (* 5 60))

; WARNING: show-alert is platform specific!
; shows Mac OS alert using Apple Script
; program execution stops unless alert is acknowledge by an user
(defun show-alert (products)
  (let* ((n-available (length products))
         (name "nvidia-stock-checker")
         (message (format nil "Found ~D available GPU(s)" n-available))
         (command (format nil "osascript -e 'display alert \"~a\" message \"~a\"'" name message)))
    (uiop:run-program command)))

(defstruct (product (:constructor create-product (title price status)))
  title
  price
  status)

(defun parse-product (json-obj)
  (create-product
    (jsown:val json-obj "productTitle")
    (jsown:val json-obj "productPrice")
    (jsown:val json-obj "prdStatus")))

(defun product-to-list (p)
  (list 
    (product-title p)
    (product-price p)
    (product-status p)))

(defun get-products (url)
  (dex:get url))

(defun parse-json (json-string)
  (let* ((json (jsown:parse json-string))
         (searched-products (jsown:val json "searchedProducts"))
         (products (jsown:val searched-products "productDetails"))
         (featured-product (jsown:val searched-products "featuredProduct"))
         (all-products (append (list featured-product) products)))
    (mapcar #'parse-product all-products)))

(defun print-product-table (products)
  (let ((table (ascii-table:make-table '("title" "price" "status"))))
    (mapcar (lambda (p) (ascii-table:add-row table (product-to-list p))) products)
    (ascii-table:display table)))

(defun filter-out-of-stock (products)
  (remove-if
    (lambda (p) (string= (product-status p) "out_of_stock"))
    products))

(defun check-availability ()
  (let* ((json-string (get-products *url*))
         (products (parse-json json-string))
         (products-by-title (sort products #'string-lessp :key #'product-title))
         (available-products (filter-out-of-stock products-by-title)))
    (if *print-product-table?* (print-product-table products-by-title))
    (if (and
          *show-alert?*
          (> (length available-products) 0))
      (show-alert available-products))))

(defun main ()
  (loop
    (check-availability)
    (when (not *loop?*) (return))
    (sleep *sleep-interval*)))