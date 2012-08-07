;;; simple-httpd.el --- HTTP/1.0 web server

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/emacs-http-server
;; Version: 1.0

;;; Commentary:

;; Use `httpd-start' to start the web server. The variable
;; `httpd-root' sets the server's root folder and `httpd-port' sets
;; the listening port. Naturally, the server needs to be restarted in
;; order for a port change to take effect.

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

(defcustom httpd-listings t
  "If true, serve directory listings."
  :group 'simple-httpd
  :type 'boolean)

(defvar httpd-mime-types
  '(("png"  . "image/png")
    ("gif"  . "image/gif")
    ("jpg"  . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("tif"  . "image/tif")
    ("tiff" . "image/tiff")
    ("css"  . "text/css")
    ("htm"  . "text/html")
    ("html" . "text/html")
    ("xml"  . "text/xml")
    ("txt"  . "text/plain")
    ("gz"   . "application/octet-stream")
    ("ps"   . "application/postscript")
    ("eps"  . "application/postscript")
    ("pdf"  . "application/pdf")
    ("tar"  . "application/x-tar")
    ("zip"  . "application/zip")
    ("mp3"  . "audio/mpeg")
    ("wav"  . "audio/x-wav")
    ("flac" . "audio/flac")
    ("spx"  . "audio/ogg")
    ("oga"  . "audio/ogg")
    ("ogg"  . "audio/ogg")
    ("ogv"  . "video/ogg")
    ("mp4"  . "video/mp4")
    ("mkv"  . "video/x-matroska")
    ("webm" . "video/webm"))
  "MIME types for headers")

(defvar httpd-indexes
  '("index.html"
    "index.htm")
  "File served by default when accessing a directory.")

(defvar httpd-status-codes
  '((200 . "OK")
    (301 . "Moved Permanently")
    (403 . "Forbidden")
    (404 . "Not Found")
    (500 . "Internal Server Error"))
  "HTTP status codes")

(defvar httpd-html
  '((403 . "<!DOCTYPE html>
<html><head>
<title>403 Forbidden</title>
</head><body>
<h1>Forbidden</h1>
<p>The requested URL is forbidden.</p>
</body></html>")
    (404 . "<!DOCTYPE html>
<html><head>
<title>404 Not Found</title>
</head><body>
<h1>Not Found</h1>
<p>The requested URL was not found on this server.</p>
</body></html>")
    (500 . "<!DOCTYPE html>
<html><head>
<title>500 Internal Error</title>
</head><body>
<h1>500 Internal Error</h1>
<p>Internal error when handling this request.</p>
</body></html>"))
  "HTML for various errors.")

;; User interface

;;;###autoload
(defun httpd-start ()
  "Start the emacs web server."
  (interactive)
  (httpd-stop)
  (httpd-clear-log)
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

;; Networking code

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
     ((file-directory-p path)
      (httpd-send-directory proc path uri-path))
     (t (httpd-send-header
         proc (httpd-get-mime (file-name-extension path)) status)
        (httpd-send-file proc path)))))

;; Logging

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
    (erase-buffer)
    (httpd-log-string "(log)\n")))

;; Request parsing

(defun httpd-parse (string)
  "Parse client http header into alist."
  (let* ((lines (split-string string "[\n\r]+"))
         (req (list (split-string (car lines)))))
    (dolist (line (cdr lines))
      (push (list (car (split-string line ": "))
                  (mapconcat 'identity
                             (cdr (split-string line ": ")) ": ")) req))
    (cddr req)))

(defun httpd-parse-uri (uri)
  "Split a URI into it's components. In the return, the first
element is the script path, the second is an alist of
variable/value pairs, and the third is the fragment."
  (let ((p1 (string-match (regexp-quote "?") uri))
        (p2 (string-match (regexp-quote "#") uri))
        retval)
    (push (if p2 (url-unhex-string (substring uri (1+ p2))))
          retval)
    (push (if p1 (mapcar (lambda (str)
                           (mapcar 'url-unhex-string (split-string str "=")))
                         (split-string (substring uri (1+ p1) p2) "&")))
          retval)
    (push (substring uri 0 (or p1 p2))
          retval)))

;; Path handling

(defun httpd-status (path)
  "Determine status code for the path."
  (cond
   ((not (file-exists-p path))   404)
   ((not (file-readable-p path)) 403)
   ((and (file-directory-p path) (not httpd-listings)) 403)
   (200)))

(defun httpd-clean-path (path)
  "Clean dangerous .. from the path and remove the leading /."
  (mapconcat 'identity
   (delete "" (delete ".." (split-string (url-unhex-string path) "/"))) "/"))

(defun httpd-gen-path (path)
  "Translate GET to secure path in httpd-root."
  (let ((clean (expand-file-name (httpd-clean-path path) httpd-root)))
    (if (file-directory-p clean)
        (let* ((dir (file-name-as-directory clean))
               (indexes (mapcar* (apply-partially 'concat dir) httpd-indexes))
               (existing (remove-if-not 'file-exists-p indexes)))
          (or (car existing) dir))
      clean)))

(defun httpd-get-mime (ext)
  "Fetch MIME type given the file extention."
  (cdr (assoc (downcase ext) httpd-mime-types)))

;; Data sending functions

(defun httpd-escape-html (string)
  "Properly encode HTML entities."
  (let ((entities '(("&" . "&amp;")
                    ("<" . "&lt;")
                    (">" . "&gt;"))))
    (reduce (lambda (s e) (replace-regexp-in-string (car e) (cdr e) s))
            entities :initial-value string)))

(defun httpd-send-header (proc mime status &rest extra-headers)
  "Send an HTTP header with given MIME type."
  (let ((status-str (cdr (assq status httpd-status-codes))))
    (with-temp-buffer
      (insert (format "HTTP/1.0 %d %s\r\n" status status-str))
      (dolist (header (cons (cons "Content-Type" mime) extra-headers))
        (insert (format "%s: %s\r\n" (car header) (cdr header))))
      (insert "\r\n")
      (process-send-string proc (buffer-string)))))

(defun httpd-send-file (proc path)
  "Serve file to the given client."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents path)
    (httpd-send-buffer proc (current-buffer))))

(defun httpd-send-directory (proc path uri-path)
  "Serve a file listing to the client."
  (let ((title (concat "Directory listing for " (httpd-escape-html uri-path))))
    (if (equal "/" (substring uri-path -1))
        (with-temp-buffer
          (httpd-send-header proc "text/html" 200)
          (set-buffer-multibyte nil)
          (insert "<!DOCTYPE html>\n")
          (insert "<html>\n<head><title>" title "</title></head>\n")
          (insert "<body>\n<h2>" title "</h2>\n<hr/>\n<ul>")
          (dolist (file (directory-files path))
            (unless (eq ?. (aref file 0))
              (if (file-directory-p (expand-file-name file path))
                  (setq file (concat file "/")))
              (let ((f (httpd-escape-html file)))
                (insert (format "<li><a href=\"%s\">%s</a></li>\n" f f)))))
          (insert "</ul>\n<hr/>\n</body>\n</html>")
          (httpd-send-buffer proc (current-buffer)))
      (httpd-send-header proc "text/plain" 301
                         (cons "Location" (concat uri-path "/")))
      (httpd-send-string proc ""))))

(defun httpd-send-string (proc string)
  "Send string to client."
  (process-send-string proc string)
  (process-send-eof proc))

(defun httpd-send-buffer (proc buffer)
  "Send buffer to client."
  (with-current-buffer buffer
    (httpd-send-string proc (buffer-substring (point-min) (point-max)))))

(defun httpd-error (proc status)
  "Handle an error situation."
  (httpd-send-header proc "text/html" status)
  (httpd-send-string proc (cdr (assq status httpd-html))))

(provide 'simple-httpd)

;;; simple-httpd.el ends here
