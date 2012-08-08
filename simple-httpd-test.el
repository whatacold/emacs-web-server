;; emacs -batch -L . -l simple-httpd-test.el -f ert-run-tests-batch

(require 'ert)
(require 'simple-httpd)

(ert-deftest httpd-escape-html-test ()
  "Test HTML escaping."
  (should (equal (httpd-escape-html "Dad & Son") "Dad &amp; Son"))
  (should (equal (httpd-escape-html "<b>bold</b>") "&lt;b&gt;bold&lt;/b&gt;"))
  (should (equal (httpd-escape-html "<a&b>") "&lt;a&amp;b&gt;")))

(ert-deftest httpd-clean-path-test ()
  "Ensure that paths are sanitized properly."
  (should (equal (httpd-clean-path "/") ""))
  (should (equal (httpd-clean-path "../") ""))
  (should (equal (httpd-clean-path "/../../foo/../..") "foo"))
  (should (equal (httpd-clean-path "/tmp/../root/foo") "tmp/root/foo")))

(ert-deftest httpd-mime-test ()
  "Test MIME type fetching."
  (should (equal (httpd-get-mime "txt") "text/plain"))
  (should (equal (httpd-get-mime "unknown") "application/octet-stream"))
  (should (equal (httpd-get-mime nil) "application/octet-stream")))

(ert-deftest httpd-parse-test ()
  "Test HTTP header parsing."
  (let* ((h "GET /f%20b HTTP/1.1\r\nHost: localhost:8080\r\DNT: 1, 2\r\n\r\n")
         (p (httpd-parse h)))
    (should (equal (cadr (assoc "GET" p)) "/f%20b"))
    (should (equal (cadr (assoc "Host" p)) "localhost:8080"))
    (should (equal (cadr (assoc "DNT" p)) "1, 2"))))

(ert-deftest httpd-parse-uri-test ()
  "Test URI parsing."
  (let* ((url "/foo/bar.html?q=test%26case&v=10#page10")
         (p (httpd-parse-uri url))
         (args (cadr p))
         (fragment (caddr p)))
    (should (equal (car p) "/foo/bar.html"))
    (should (equal (cadr (assoc "v" args)) "10"))
    (should (equal (cadr (assoc "q" args)) "test&case"))
    (should (equal fragment "page10"))))

(ert-deftest httpd-send-header-test ()
  "Test server header output."
  (let ((header ""))
    (flet ((process-send-string (proc str) (setq header (concat header str))))
      (httpd-send-header nil "text/html" 404 (cons "Foo" "bar")))
    (let ((out (httpd-parse header)))
      (should (equal (cadr (assoc "HTTP/1.0" out)) "404"))
      (should (equal (cadr (assoc "Content-Type" out)) "text/html"))
      (should (equal (cadr (assoc "Foo" out)) "bar")))))

(ert-deftest httpd-status-test ()
  "Test HTTP status message for mocked request states."
  (flet ((file-exists-p (file) t)
         (file-readable-p (file) nil))
    (should (eq (httpd-status "/some/file") 403)))
  (flet ((file-exists-p (file) nil))
    (should (eq (httpd-status "/some/file") 404)))
  (flet ((file-exists-p (file) t)
         (file-readable-p (file) t)
         (file-directory-p (file) nil))
    (should (eq (httpd-status "/some/file") 200)))
  (flet ((file-exists-p (file) t)
         (file-readable-p (file) t)
         (file-directory-p (file) t))
    (let ((httpd-listings nil))
      (should (eq (httpd-status "/some/file") 403)))
    (let ((httpd-listings t))
      (should (eq (httpd-status "/some/file") 200)))))
