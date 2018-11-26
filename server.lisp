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
    response)
  
  (format nil "You requested ~a, ~a; with request body ~a"
          url
          timeout
          ;; notice that the raw request is in `hunchentoot:*request*`
          (hunchentoot:raw-post-data :force-text t)))

;;; PLAYING AROUND:

;; more options at: https://edicl.github.io/drakma/#http-request
;; (defparameter *last-request* (drakma:http-request "https://xml-mockery.herokuapp.com/card_service"
;;                                                   :method :post
;;                                                   :content "<TransferredValueTxn><TransferredValueTxnReq> <ReqCat>TransferredValue</ReqCat> <ReqAction>Echo</ReqAction> <Date>20181017</Date> <Time>180000</Time> <PartnerName>PARTNER</PartnerName> <EchoData>test</EchoData></TransferredValueTxnReq> </TransferredValueTxn>"
;;                                                   :content-type "text/xml"))

;; FOR REPL INTERACTION:
(hunchentoot:start *server*)
(hunchentoot:stop *server*)
