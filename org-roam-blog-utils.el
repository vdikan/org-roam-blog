;;; org-roam-blog-utils.el --- Utility definitions used throughout ORB -*- lexical-binding: t; -*-

(require 'unidecode)

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

(defsubst org-roam-blog--htmlize-node-content (node)
  "Get a node content and HTMLize it with preferred engine."
  (org-roam-blog--org-to-html
   (with-current-buffer (org-roam-node-find-noselect node t)
     (when-let ((beg (point))
                (end (progn (outline-end-of-subtree) (point))))
       (buffer-substring-no-properties beg end)))))

(defsubst org-roam-blog--drop-main-tag (s)
  "Remove <main> tag from orgize-produced html."
  (seq-subseq s 6 (- (length s) 7)))

;;;; Footer

(provide 'org-roam-blog-utils)

;;; org-roam-blog-utils.el ends here
