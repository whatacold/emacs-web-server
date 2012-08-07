;;; simple-httpd.el --- HTTP/1.0 web server

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/emacs-http-server
;; Version: 1.0

;;; Commentary:

;; Use `httpd-start' to start the web server. The variable
;; `httpd-root' sets the server's root folder and `httpd-port' sets
;; the listening port.

;;; TODO:

;; * Directory listing

;;; Code:

(require 'cl)

(defgroup simple-httpd nil
  "A simple web server."
  :group 'comm)

(defcustom httpd-port 8080
  "Web server port."
  :group 'simple-httpd
  :type 'integer)

(defcustom httpd-root "~/public_html"
  "Web server file root."
  :group 'simple-httpd
  :type 'directory)

(defvar httpd-mime-types
  '(("png"  . "image/png")
    ("gif"  . "image/gif")
    ("jpg"  . "image/jpg")
    ("css"  . "text/css")
    ("htm"  . "text/html")
    ("html" . "text/html")
    ("xml"  . "text/xml")
    ("txt"  . "text/plain"))
  "MIME types for headers")

(defvar httpd-indexes
  '("index.html"
    "index.htm")
  "File served by default when accessing a directory.")

(defvar httpd-status-codes
  '((200 . "OK")
    (403 . "Forbidden")
    (500 . "Internal Server Error")
    (404 . "Not Found"))
  "HTTP status codes")

(defvar httpd-html
  '((404 . "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>404 Not Found</title>
</head><body>
<h1>Not Found</h1>
<p>The requested URL was not found on this server.</p>
</body></html>")
    (403 . "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>403 Forbidden</title>
</head><body>
<h1>Forbidden</h1>
<p>The requested URL is forbidden.</p>
</body></html>")
    (500 . "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>500 Internal Error</title>
</head><body>
<h1>500 Internal Error</h1>
<p>Internal error when handling this request.</p>
</body></html>"))
  "HTML for various errors.")

;;;###autoload
(defun httpd-start ()
  "Start the emacs web server."
  (interactive)
  (httpd-stop)
  (httpd-clear-log)
  (httpd-log-string "(log)\n")
  (httpd-log-alist `(start ,(current-time-string)))
  (make-network-process
   :name     "httpd"
   :service  httpd-port
   :server   t
   :family   'ipv4
   :filter   'httpd-filter))

;;;###autoload
(defun httpd-stop ()
  "Stop the emacs web server."
  (interactive)
  (if (process-status "httpd") (delete-process "httpd"))
  (httpd-log-alist `(stop ,(current-time-string))))

(defun httpd-log-string (string)
  "Add string to the web server log."
  (with-current-buffer (get-buffer-create "*httpd*")
    (goto-char (point-max))
    (insert string)))

(defun httpd-log-alist (item &optional sp)
  "Add alist to the log."
  (if (not sp) (setq sp 2))
  (with-current-buffer (get-buffer-create "*httpd*")
    (goto-char (- (point-max) 2))
    (insert "\n" (make-string sp 32))
    (if (atom (cadr item)) (insert (format "%S" item))
      (insert "(" (symbol-name (car item)))
      (dolist (el (cdr item))
        (httpd-log-alist el (+ 1 sp)))
      (insert ")"))))

(defun httpd-clear-log ()
  "Clear the web server log."
  (with-current-buffer (get-buffer-create "*httpd*")
    (erase-buffer)))

(defun httpd-filter (proc string)
  "Runs each time client makes a request."
  (let* ((log '(connection))
         (req (httpd-parse string))
         (uri (cadr (assoc "GET" req)))
         (parsed-uri (httpd-parse-uri uri))
         (uri-path (nth 0 parsed-uri))
         (uri-query (nth 1 parsed-uri))
         (uri-target (nth 2 parsed-uri))
         (path (httpd-gen-path uri-path))
         (status (httpd-status path)))
    (setq log (list 'connection
                    `(date ,(current-time-string))
                    `(address ,(car (process-contact proc)))
                    `(get ,uri-path)
                    (append '(req) req)
                    `(path ,path)
                    `(status ,status)))
    (httpd-log-alist log)
    (cond
     ((not (= status 200)) (httpd-error proc status))
     (t (httpd-send-header proc (httpd-get-mime (httpd-get-ext path)) status)
        (httpd-send-file proc path)))))

(defun httpd-parse (string)
  "Parse client http header into alist."
  (let* ((lines (split-string string "[\n\r]+"))
         (req (list (split-string (car lines)))))
    (dolist (line (cdr lines))
      (push (list (car (split-string line ": "))
                  (mapconcat 'identity
                             (cdr (split-string line ": ")) ": ")) req))
    (cddr req)))

(defun httpd-status (path)
  "Determine status code for the path."
  (cond
   ((not (file-exists-p path))   404)
   ((not (file-readable-p path)) 403)
   ((file-directory-p path)      403)
   (200)))

(defun httpd-gen-path (path)
  "Translate GET to secure path in httpd-root."
  (let ((clean (expand-file-name (httpd-clean-path path) httpd-root)))
    (if (file-directory-p clean)
        (let* ((dir (file-name-as-directory clean))
               (indexes (mapcar* (apply-partially 'concat dir) httpd-indexes))
               (existing (remove-if-not 'file-exists-p indexes)))
          (or (car existing) dir))
      clean)))

(defun httpd-send-file (proc path)
  "Serve file to the given client."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents path)
    (httpd-send-buffer proc (current-buffer))))

(defun httpd-clean-path (path)
  "Clean dangerous .. from the path and remove the leading /."
  (mapconcat 'identity
   (delete "" (delete ".." (split-string (url-unhex-string path) "/"))) "/"))

(defun httpd-get-ext (path)
  "Get extention from path to determine MIME type."
  (car (reverse (split-string path "\\."))))

(defun httpd-get-mime (ext)
  "Fetch MIME type given the file extention."
  (cdr (assoc ext httpd-mime-types)))

(defun httpd-send-header (proc mime status)
  "Send header with given MIME type."
  (let ((status-str (cdr (assq status httpd-status-codes))))
    (process-send-string
     proc (format "HTTP/1.0 %d %s\nContent-Type: %s\n\n"
                  status status-str mime))))

(defun httpd-error (proc status)
  "Handle an error situation."
  (httpd-send-header proc "text/html" status)
  (httpd-send-string proc (cdr (assq status httpd-html))))

(defun httpd-send-string (proc string)
  "Send string to client."
  (process-send-string proc string)
  (process-send-eof proc))

(defun httpd-send-buffer (proc buffer)
  "Send buffer to client."
  (with-current-buffer buffer
    (httpd-send-string proc (buffer-substring (point-min)
                                              (point-max)))))

(defun httpd-parse-uri (uri)
  "Split a URI into it's components. In the return, the first
element is the script path, the second is an alist of
variable/value pairs, and the third is the target."
  (let ((p1 (string-match (regexp-quote "?") uri))
        (p2 (string-match (regexp-quote "#") uri))
        retval)
    (push (if p2 (url-unhex-string (substring uri (1+ p2))))
          retval)
    (push (if p1 (mapcar #'(lambda (str)
                             (mapcar 'url-unhex-string (split-string str "=")))
                         (split-string (substring uri (1+ p1) p2) "&")))
          retval)
    (push (substring uri 0 (or p1 p2))
          retval)))

(provide 'httpd)

;;; simple-httpd.el ends here
