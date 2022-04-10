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

(defsubst org-roam-node--lead-index-for (node-id)
 (ht-get -orb--entry-registry node-id))

(defsubst org-roam-blog--backlinks-to-context (node)
  "Extract backlinks for a context of the NODE."
  (let* ((backlinks (org-roam-backlinks-get node))
         (backnodes (mapcar #'org-roam-backlink-source-node backlinks)))
    (cl-loop for bn in backnodes
             for lead-index = (org-roam-node--lead-index-for (org-roam-node-id bn))
             when lead-index  ; process those back-linked entries that are on this site as well
             collect (ht ("title" (org-roam-node-title bn))
                         ("link" (org-roam-blog--relative-entry-url
                                  bn lead-index))))))

(defun org-roam-blog--prepr-replace-node-links (text)
  (cl-labels ((link-replace ()
                (if-let ((id-link (org-element-map (org-element-parse-buffer) 'link
                                    (lambda (link)
                                      (when (string= (org-element-property :type link) "id")
                                        (list
                                         (org-element-property :path link)
                                         (org-element-property :begin link)
                                         (org-element-property :end link)
                                         (org-element-property :contents-begin link)
                                         (org-element-property :contents-end link))))
                                    nil t)))
                    (if-let ((index (org-roam-node--lead-index-for (first id-link))))
                        (setf (buffer-substring (second id-link) (third id-link))
                              (format "[[%s][%s]]"
                                      (org-roam-blog--relative-entry-url
                                       (org-roam-node-from-id (first id-link))
                                       index)
                                      (buffer-substring (fourth id-link) (fifth id-link))))
                      (setf (buffer-substring (second id-link) (third id-link))
                            (format "[[%s][%s]]" (first id-link)
                                    (buffer-substring (fourth id-link) (fifth id-link))))))))
    (with-temp-buffer
      (insert text)
      (let ((links-to-go t))
        (while links-to-go
          (setf links-to-go (link-replace))))
      (buffer-string))))

(defun org-roam-blog--prepr-filter-noexport (text)
  (cl-labels
      ((-filter-noexport
        () (let ((p (point)))
             (setf p (re-search-forward "^*" nil t))
             (when p
               (goto-char p)
               (when (member "noexport" (org-get-tags))
                 (org-cut-subtree)))
             p)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((scan (point)))
        (while (not (null scan))
          (setf scan (-filter-noexport))))
      (buffer-string))))

(defun org-roam-blog--preprocess-node-content (text)
  (-> text
    (org-roam-blog--prepr-filter-noexport)
    (org-roam-blog--prepr-replace-node-links)))

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
      (funcall htmlizer
               (org-roam-blog--preprocess-node-content
                (org-roam-blog--get-node-content node))))))

(defsubst org-roam-blog--drop-main-tag (s)
  "Remove <main> tag from orgize-produced html."
  (seq-subseq s 6 (- (length s) 7)))

;;;; Footer

(provide 'org-roam-blog-utils)

;;; org-roam-blog-utils.el ends here
