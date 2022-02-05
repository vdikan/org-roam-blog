;;; org-roam-blog-utils.el --- Utility definitions used throughout ORB -*- lexical-binding: t; -*-

(require 'org-roam)
(require 'unidecode)
(require 'f)

(defsubst org-roam-blog--get-node-property (node property)
  "Get PROPERTY from Roam NODE; 'assocdr'-style shortcut."
  (declare (type org-roam-node node)
           (type string property))
  (--> node
       (org-roam-node-properties it)
       (assoc property it)
       (cdr it)))

(defun org-roam-blog--current-dir ()
  "Get the absolute path of the current directory."
  (expand-file-name
   (if load-file-name
       (file-name-directory load-file-name)
     default-directory)))

(defsubst org-roam-blog--substring-from-right (s n)
  "Retusrns substring of length N from the end of string S."
  (let* ((end (length s)) (beg (- end n)))
    (substring s beg end)))

(defsubst fname-with-tslash (fname)
  "Make sure that FNAME comes with trailing slash."
  (let ((fname (string-trim fname)))
    (if (string-equal "/" (org-roam-blog--substring-from-right fname 1))
        fname
      (format "%s/" fname))))

(defsubst fname-no-tslash (fname)
  "Make sure that FNAME comes with trailing slash."
  (let ((fname (string-trim fname)))
    (if (string-equal "/" (org-roam-blog--substring-from-right fname 1))
        (substring fname 0 -1)
      fname)))

(defsubst org-roam-blog-to-calendar-date (s)
  "Constructs a valid date list for Calendar from Org's datestring S."
  (let ((dt (org-parse-time-string s)))
    (list (nth 4 dt)     ; month
          (nth 3 dt)     ; day
          (nth 5 dt))))  ; year

(defsubst org-roam-blog-slugify (string)
  "Default slug builder. Sanitizes the STRING using `unidecode'
package, removes extra hyphens, coerces result to lowercase."
  (downcase
   (seq-reduce
    (lambda (accum item)
      (replace-regexp-in-string (car item) (cdr item)  accum))
    '(("--+" . "-") ("^-"  . "") ("-$"  . ""))
    (unidecode-sanitize string))))

;; Loads my testing dynamic module:
(load-rs-module "my_org_dynmod")

;; that priovides `my-org-dynmod/org-to-html'

(defsubst org-roam-blog--org-to-html (s)
  (funcall #'my-org-dynmod/org-to-html s))

(defsubst org-roam-blog--get-node-html-fn (node)
  "When Roam NODE's html-fn-property is set, return its symbol interned."
  (declare (type org-roam-node node))
  (--> node
       (org-roam-blog--get-node-property
        it org-roam-blog-html-fn-property)
       (when it (intern it))))

(defun org-roam-blog--content-start ()
  (if (re-search-forward
        org-roam-blog-outline-content-start-regexp nil 'move)
      (forward-char -1)))

(defsubst org-roam-blog--get-node-content (node)
  "Get textual content of a NODE, for either headline or a file NODE."
  (with-current-buffer (org-roam-node-find-noselect node t)
    (let* ((beg (condition-case nil
                    (progn (outline-back-to-heading) (point))
                  (error 1)))
           (end (if (= beg 1)
                    (point-max)
                  (progn (outline-end-of-subtree) (point))))
           (beg (progn
                  (goto-char beg)
                  (org-roam-blog--content-start)
                  (point))))
        (buffer-substring-no-properties beg end))))

(defsubst org-roam-blog--htmlize-node-content (node)
  "Get a node content and HTMLize it with preferred engine."
  (let ((htmlizer (or (org-roam-blog--get-node-html-fn node)
                      #'org-roam-blog--org-to-html)))
    (if-let ((raw-content-filename
              (org-roam-blog--get-node-property
               node org-roam-blog-html-src-property)))
        (with-temp-buffer
          (insert-file-contents
           (f-expand raw-content-filename
                      (f-dirname (org-roam-node-file node))))
          (buffer-string))
      (funcall htmlizer (org-roam-blog--get-node-content node)))))

(defsubst org-roam-blog--drop-main-tag (s)
  "Remove <main> tag from orgize-produced html."
  (seq-subseq s 6 (- (length s) 7)))

;;;; Footer

(provide 'org-roam-blog-utils)

;;; org-roam-blog-utils.el ends here
