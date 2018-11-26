;; load our sbclrc to get quicklisp
(load "/home/luis/.sbclrc")

;; Load dependencies

;; for http serving: https://edicl.github.io/hunchentoot/
(ql:quickload :hunchentoot)

;; for http requests: https://edicl.github.io/drakma/
(ql:quickload :drakma)

;; If we don't use easy-acceptor (and instead use acceptor),
;; handlers defined by define-easy-handler won't be picked up.
(defparameter *server* (make-instance 'hunchentoot:easy-acceptor :port 8666))

;; Main handler
(hunchentoot:define-easy-handler (proxy-request :uri "/proxy-request") (url timeout)
  ;; if they want a timeout, assume they meant minutes and do just that:
  (let ((timeout-n (parse-integer (or timeout "0"))))
    (if (and timeout (numberp timeout-n))
        (sleep (* 60 timeout-n))))

  ;; make the request on their behalf, and return whatever came back:
  (let ((response (handle-request url
                                  (hunchentoot:raw-post-data :force-text t))))
    (setf (hunchentoot:content-type*) "text/xml")
    response))

;; This could be made fancier by also sending over the headers, etc, but we have some
;; knowledge of what exactly we're dealing with here (a crappy server that don't care 'bout nothin')
(defun handle-request (url body)
  ;; drakma:http-request actually returns multiple values, we could look into
  ;; leveraging those to return the right status code, headers, etc.
  ;; more options at: https://edicl.github.io/drakma/#http-request
  (let ((response (drakma:http-request url
                                       :method :post
                                       :content body
                                       :content-type "text/xml")))
    response))

(hunchentoot:start *server*)
;;(hunchentoot:stop *server*)
