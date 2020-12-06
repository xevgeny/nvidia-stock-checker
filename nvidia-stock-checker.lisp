(in-package #:nvidia-stock-checker)

(defvar url "https://api.nvidia.partners/edge/product/search?page=1&limit=9&locale=de-de&manufacturer=NVIDIA")

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
    (mapcar #'(lambda (p) (ascii-table:add-row table (product-to-list p))) products)
    (ascii-table:display table)))

(defun check-availability ()
  (let* ((json-string (get-products url))
         (products (parse-json json-string))
         (by-title (sort products #'string-lessp :key #'product-title)))
    (print-product-table by-title)))
