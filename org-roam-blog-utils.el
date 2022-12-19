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

(defun org-roam-node-find-noselect (node &optional force)
  "Navigate to the point for NODE, and return the buffer.
If NODE is already visited, this won't automatically move the
point to the beginning of the NODE, unless FORCE is non-nil."
  (unless (org-roam-node-file node)
    (user-error "Node does not have corresponding file"))
  (let ((buf (find-file-noselect (org-roam-node-file node))))
    (with-current-buffer buf
      (when (or force
                (not (equal (org-roam-node-id node)
                            (org-roam-id-at-point))))
        (goto-char (org-roam-node-point node))))
    buf))

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

;; that provides `my-org-dynmod/org-to-html'

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
 (ht-get (org-roam-blog-g-entries-get) node-id))

(defsubst org-roam-blog--backlinks-to-context (node)
  "Extract backlinks for a context of the NODE."
  (let* ((backlinks (org-roam-backlinks-get node))
         (backnodes (mapcar #'org-roam-backlink-source-node backlinks)))
    (->
     (cl-loop for bn in backnodes
              for lead-index = (org-roam-node--lead-index-for (org-roam-node-id bn))
              when lead-index ; process those back-linked entries that are on this site as well
              collect (ht ("title" (org-roam-node-title bn))
                          ("link" (org-roam-blog--relative-entry-url
                                   bn lead-index))))
     (remove-duplicates :test #'ht-equal?))))

(defun org-roam-blog--toc-to-context (node req-level)
  (let* ((lead-index (org-roam-node--lead-index-for
                      (org-roam-node-id node)))
         (urlprefix (org-roam-blog--relative-entry-url node lead-index))
         (headlines
          (with-temp-buffer
            (insert (org-roam-blog--prepr-filter-noexport
                     (org-roam-blog--get-node-content node)))
            (org-element-map (org-element-parse-buffer) 'headline
              (lambda (headline)
                (let ((title (org-element-property :raw-value headline))
                      (level (org-element-property :level headline))
                      (begin (org-element-property :begin headline)))
                  (when
                      (and
                       (string-match
                        org-roam-blog-sure-headline-regex
                        (progn (goto-char begin) (thing-at-point 'line t)))
                       (not
                        (string-match org-roam-blog-anchor-regex title))) 
                    (cons title level))))))))
    (->> (cl-loop for hl in headlines
                  for i from 1
                  collect (cons hl i))
         (remove-if-not (lambda (hl) (>= req-level (cdar hl))))
         (mapcar (lambda (hl)
                   (ht ("title" (caar hl))
                       ("link"
                        (format "%s#%s-%s"
                                urlprefix
                                (org-roam-blog-slugify (caar hl))
                                (cdr hl)))))))))

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

(defun org-roam-blog--prepr-replace-node-links (text)
    (cl-labels ((keep-trailing-space
                 (point)
                 (if (char-equal 32 (char-before point)) " " ""))
                (link-replace ()
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
                                (format "[[%s][%s]]%s"
                                        (org-roam-blog--relative-entry-url
                                         (org-roam-node-from-id (first id-link))
                                         index)
                                        (buffer-substring (fourth id-link) (fifth id-link))
                                        (keep-trailing-space (third id-link))))
                        (setf (buffer-substring (second id-link) (third id-link))
                              (format "[[%s][%s]]" (first id-link)
                                      (buffer-substring (fourth id-link) (fifth id-link))))))))
      (with-temp-buffer
        (insert text)
        (let ((links-to-go t))
          (while links-to-go
            (setf links-to-go (link-replace))))
        (buffer-string))))

(defun org-roam-blog--prepr-media-links (text node)
  (let* ((index  (org-roam-node--lead-index-for
                  (org-roam-node-id node)))
         (destdir (org-roam-blog-site-scratch-dir (org-roam-blog-g-site-get)))
         (destdir (f-expand (org-roam-blog-index-slug index) destdir))
         (destdir (f-expand (org-roam-blog-index-media-dir index) destdir))
         (urlprefix (concat "/" (org-roam-blog-index-slug index)
                            "/" (org-roam-blog-index-media-dir index))))
    (unless (f-exists-p destdir)
      ;; FIXME: some setups have outdated f.el:
      ;; (f-mkdir-full-path destdir)
      (shell-command (format "mkdir -p %s" destdir)))
    (cl-labels
        ((link-replace
          ()
          (if-let ((file-link
                    (org-element-map (org-element-parse-buffer) 'link
                      (lambda (link)
                        (when (string= (org-element-property :type link) "file")
                          (list
                           (org-element-property :path link)
                           (org-element-property :begin link)
                           (org-element-property :end link))))
                      nil t)))
              (cl-destructuring-bind (path begin end) file-link
                (let ((path-orig
                       (f-expand path (f-dirname (org-roam-node-file node))))
                      (path-dest
                       (f-expand (f-filename path) destdir))
                      (urlpath (concat urlprefix "/" (f-filename path))))
                  (when (f-exists? path-orig)
                    (unless (f-exists? path-dest) (f-copy path-orig path-dest))
                    (if (member (downcase (f-ext path)) org-roam-blog-media-image-extensions)
                        (setf (buffer-substring begin end)
                              (format org-roam-blog-media-image-inline-format-string
                                      urlpath))
                      (setf (buffer-substring begin end)
                            (format "[[%s][%s]]" urlpath (f-filename path))))))))))
      (with-temp-buffer
        (insert text)
        (let ((links-to-go t))
          (while links-to-go
            (setf links-to-go (link-replace))))
        (buffer-string)))))

(defun org-roam-blog--prepr-prepend-anchor-links (text node)
    (let ((idx 0))
      (cl-labels
          ((to-id (title idx)
                  (format org-roam-blog-anchor-id-format
                          (org-roam-blog-slugify title)
                          idx))
           (to-anchor (title idx)
                      (format org-roam-blog-anchor-format
                              (org-roam-blog-slugify title)
                              idx))                                 
           (prepend-anchor-link
            ()
            (if-let ((headline
                      (org-element-map (org-element-parse-buffer) 'headline
                        (lambda (headline)
                          (let ((title (org-element-property :raw-value headline))
                                (level (org-element-property :level headline))
                                (begin (org-element-property :begin headline)))
                            (when
                                (and
                                 (string-match
                                  org-roam-blog-sure-headline-regex
                                  (progn (goto-char begin) (thing-at-point 'line t)))
                                 (not
                                  (string-match org-roam-blog-anchor-regex title))) 
                                (list title level begin))))
                        nil t)))
                (cl-destructuring-bind (title level begin) headline
                  (cl-incf idx)
                  (let* ((anchor (to-anchor title idx))
                         (id (to-id title idx)))
                    (setf (buffer-substring (+ begin level) (+ begin level))
                          anchor)
                    (setf (buffer-substring begin begin)
                          (format "#+begin_export html\n<a id=\"%s\"></a>\n#+end_export\n"
                                  id)))))))
        (with-temp-buffer
          (insert text)
          (let ((headlines-to-go t))
            (while headlines-to-go
              (setf headlines-to-go (prepend-anchor-link))))
          (buffer-string)))))

(defun org-roam-blog--preprocess-node-content (text node)
  (-> text
    (org-roam-blog--prepr-filter-noexport)
    (org-roam-blog--prepr-media-links node)
    (org-roam-blog--prepr-replace-node-links)
    (org-roam-blog--prepr-prepend-anchor-links node)))

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
                (org-roam-blog--get-node-content node) node)))))

(defsubst org-roam-blog--drop-main-tag (s)
  "Remove <main> tag from orgize-produced html."
  (seq-subseq s 6 (- (length s) 7)))

;;;; Footer

(provide 'org-roam-blog-utils)

;;; org-roam-blog-utils.el ends here
