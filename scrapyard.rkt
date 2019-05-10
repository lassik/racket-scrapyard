#lang racket/base

;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(require json
         net/url
         racket/file
         racket/function
         racket/list
         racket/port
         racket/string
         xml)

(require html-parsing
         sxml
         txexpr)

;;

;; Users can change this if they don't like the default.
(define cache-dir (build-path (current-directory) ".cache"))

;; Usage: (cache-http cache-name url [keyword-args])
;; Any keyword-args are passed to http-sendrecv/url.
(define cache-http
  (make-keyword-procedure
   (lambda (kw-syms kw-args cache-name url)
     (let ((url (if (url? url) url (string->url url)))
           (cache-path (build-path cache-dir cache-name)))
       (make-directory* cache-dir)
       (unless (file-exists? cache-path)
         (fprintf (current-error-port) "Downloading <~a> from <~a>...~%"
                  cache-name (url->string url))
         (let-values
             ([(status headers input)
               (keyword-apply http-sendrecv/url kw-syms kw-args (list url))])
           (unless (bytes=? status #"HTTP/1.1 200 OK")
             (error "HTTP request did not return 200 OK"))
           (call-with-atomic-output-file
            cache-path (lambda (output _) (copy-port input output)))
           (close-input-port input)))
       cache-path))))

;;

(define (shebang-line? line)
  (string-prefix? line "#!"))

(define (read-all-into-list)
  (let loop ((xs '()))
    (let ((x (read)))
      (if (eof-object? x) xs (loop (append xs (list x)))))))

;;

;; Example: (scrape-html (cache-http "example.html" "https://example.com/"))

(define (scrape-bytes path)
  (call-with-input-file path port->bytes))

(define (scrape-string path)
  (call-with-input-file path port->string #:mode 'text))

(define (scrape-lines path)
  (call-with-input-file path port->lines #:mode 'text))

(define (scrape-xml path)
  (call-with-input-file path read-xml #:mode 'text))

(define (scrape-html path)
  (html->xexp (scrape-string path)))

(define (scrape-sexp path)
  (with-input-from-string
    (string-join (dropf (scrape-lines path) shebang-line?) "\n")
    read-all-into-list))

(define (scrape-json-predicate predicate path)
  (let ((json-data (call-with-input-file path read-json)))
    (cond ((eof-object? json-data)
           (error "No data"))
          ((and predicate (not (predicate json-data)))
           (error "Bad data" json-data))
          (else json-data))))

(define scrape-json (curry scrape-json-predicate #f))

(define scrape-json-list (curry scrape-json-predicate list?))

(define scrape-json-hash (curry scrape-json-predicate hash-eq?))

;;

(provide
 cache-dir
 cache-http

 scrape-bytes
 scrape-string
 scrape-lines
 scrape-xml
 scrape-html
 scrape-sexp
 scrape-json-predicate
 scrape-json
 scrape-json-list
 scrape-json-hash)
