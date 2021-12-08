;;; org-roam-blog-site.el --- ORB's Site definitions  -*- lexical-binding: t; -*-

(require 'mustache)
(require 'ht)
(require 'f)
(require 'org-roam-blog-utils)
(require 'org-roam-blog-index)

(defun org-roam-blog--content-wrap-default (site template context)
  "Default content wrapper for ORB SITE. Renders CONTENT in TEMPLATE
placed between `wrapper-top` and `wrapper-bottom` mustache partials.
They must exist inside `src-template-dir' of the SITE.
Returns a closure with a signature expected by `mustache-render' for
\"wrapper\" tags."
  (let ((template-dirlist
         (list (org-roam-blog-site-src-template-dir site))))
    (lambda (template context)
      (concat (let ((mustache-partial-paths template-dirlist))
                (mustache-render "{{> wrapper-top}}" context))
              (mustache-render template context)
              (let ((mustache-partial-paths template-dirlist))
                (mustache-render "{{> wrapper-bottom}}" context))))))


(defun org-roam-blog--render-default (site template context)
  "Default render function for ORB SITE. Merges CONTEXT with
`site-top-context' and processes through wrapped TEMPLATE."
  (mustache-render
   (concat "{{#orb-site-wrapped}}" template "{{/orb-site-wrapped}}")
   (ht-merge
    context
    (org-roam-blog-site-top-context site)
    (ht ("orb-site-wrapped"
         (funcall #'org-roam-blog--content-wrap-default
                  site template context))))))

(cl-defstruct (org-roam-blog-site (:constructor org-roam-blog-site--create)
                                  (:copier nil))
  (staging-dir        nil          :type string)
  (src-root-dir       nil          :type string)
  (src-template-dir   nil          :type string)
  (index-ht           (ht-create)  :type hash-table)
  (top-context        (ht-create)  :type hash-table)
  (content-wrap-fn    #'org-roam-blog--content-wrap-default
                      :type function)
  (render-fn          #'org-roam-blog--render-default
                      :type function))

(cl-defun org-roam-blog-site-create (&rest args)
  (let ((site (apply #'org-roam-blog-site--create args)))
    (unless (org-roam-blog-site-staging-dir site)
      (setf (org-roam-blog-site-staging-dir site)
            (read-directory-name "Select staging directory (output location):")))

    (unless (org-roam-blog-site-src-root-dir site)
      (setf (org-roam-blog-site-src-root-dir site)
            (read-directory-name "Select root files source directory:")))

    (unless (org-roam-blog-site-src-template-dir site)
      (setf (org-roam-blog-site-src-template-dir site)
            (read-directory-name "Select template files source directory:")))
    site))

(defsubst org-roam-blog-render (site template context)
  "Public shortcut to wrap and output CONTEXT to specific
content mustache TEMPLATE for a SITE."
  (funcall (org-roam-blog-site-render-fn site)
           site template context))

(defsubst org-roam-blog-stage (fname site template context &optional subdir)
  "Output rendered page as FNAME file under `staging-dir' of the SITE.
Extend the staging path with SUBDIR when specified.
Pipes SITE, TEMPLATE and CONTEXT through `org-roam-blog-render'."
  (let* ((subdir (or subdir ""))
         (subdir (f-expand subdir (org-roam-blog-site-staging-dir site)))
         (_ (f-mkdir subdir))
         (staging-path (f-expand fname subdir)))
    (f-write-text (org-roam-blog-render site template context)
                  'utf-8 staging-path)))

(defsubst org-roam-blog-register-index (site index)
    "Register INDEX in the index-ht of the SITE,
using slug of the INDEX as key."
    (ht-set!
     (org-roam-blog-site-index-ht site)
     (org-roam-blog-index-slug index)
     index))

(defsubst org-roam-blog-site--stage-index (site index)
  (if (and (org-roam-blog-index-template index)
           (org-roam-blog-index-context-fn index))
      (let ((template (org-roam-blog-index-template index))
            (context (funcall
                      (org-roam-blog-index-context-fn index)
                      index))
            (subdir (org-roam-blog-index-slug index)))
        (loop for context-group in context
              do (org-roam-blog-stage
                  (format "index-%s.html" (ht-get context-group "page")) ;<-variable
                  site template context-group subdir))
        ;;FIXME: symlink for the first "index" output?:
        (org-roam-blog-stage "index.html" site template (car context) subdir)) ;<-variable
    (block no-index-page-output
      (when (null (org-roam-blog-index-template index))
        (message "no template defined for %s"
                 (org-roam-blog-index-title index)))
      (when (null (org-roam-blog-index-context-fn index))
        (message "no context-fn defined for %s"
                 (org-roam-blog-index-title index))))))

(defsubst org-roam-blog-stage-site (site)
  "Main staging routine for a SITE."
  (loop for index in (ht-values (org-roam-blog-site-index-ht site))
        do (block stage-index
             (message "staging index %s" (org-roam-blog-index-title index))
             (org-roam-blog-site--stage-index site index))))

;;;; Footer

(provide 'org-roam-blog-site)

;;; org-roam-blog-site.el ends here
