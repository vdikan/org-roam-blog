;;; org-roam-blog-site.el --- ORB's Site definitions  -*- lexical-binding: t; -*-

(require 'mustache)
(require 'ht)
(require 'f)
(require 'simple-httpd)
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
      (let ((mustache-partial-paths template-dirlist))
        (concat
         (mustache-render "{{> wrapper-top}}" context)
         (mustache-render template context)
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
  (scratch-dir        nil          :type string)
  (src-root-dir       nil          :type string)
  (src-template-dir   nil          :type string)
  (index-ht           (ht-create)  :type hash-table)
  (top-context        (ht-create)  :type hash-table)
  (entry-registry     (ht-create)  :type hash-table)
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
  "Output rendered page as FNAME file under `scratch-dir' of the SITE.
Extend the staging path with SUBDIR when specified.
Pipes SITE, TEMPLATE and CONTEXT through `org-roam-blog-render'.

Note that the `scratch-dir' is meant to be (r)sync-ed with the final
`staging-dir' by the top-level SITE generating routines like e.g.
`org-roam-blog-stage-site', that will create/process/sync/erase another
`scratch-dir' (setting it to nil for the processed site). For debugging
the output of particular pages/sections of the SITE through calls to
`org-roam-blog-stage', set its `scratch-dir' field manually."
  (let* ((subdir (or subdir ""))
         (subdir (f-expand subdir (org-roam-blog-site-scratch-dir site)))
         (_ (f-mkdir subdir))
         (scratch-path (f-expand fname subdir)))
    (f-write-text (org-roam-blog-render site template context)
                  'utf-8 scratch-path)))

(defsubst org-roam-blog-register-index (site index)
    "Register INDEX in the index-ht of the SITE,
using slug of the INDEX as key."
    (ht-set!
     (org-roam-blog-site-index-ht site)
     (org-roam-blog-index-slug index)
     index))

(defmacro org-roam-blog-reg (site &rest kwargs)
  "Shortcut macro for immediate registration of Org Roam Blog index
defined by KWARGS for the specified SITE."
  (let ((index (gensym "orb-index-")))
    `(let ((,index (org-roam-blog-index-create ,@kwargs)))
       (org-roam-blog-register-index ,site ,index))))

(defsubst org-roam-blog-site--stage-index-groups (site index &optional entry-list)
  "Stage the grouped preview pages (feeds) for the INDEX of a SITE.
ENTRY-LIST may come optionally pre-built."
  (if (and (org-roam-blog-index-template index)
           (org-roam-blog-index-context-fn index))
      (let ((template (org-roam-blog-index-template index))
            (context (funcall
                      (org-roam-blog-index-context-fn index)
                      index entry-list))
            (subdir (org-roam-blog-index-slug index)))
        (cl-loop for context-group in context
                 do (org-roam-blog-stage
                     (format "%s-%s.html"
                             org-roam-blog-index-filename-prefix
                             (ht-get context-group "page"))
                     site template context-group subdir))
        (org-roam-blog-stage (format "%s.html" org-roam-blog-index-filename-prefix)
                             site template (car context) subdir))
    (block no-index-page-output
      (when (null (org-roam-blog-index-template index))
        (message "no template defined for %s"
                 (org-roam-blog-index-title index)))
      (when (null (org-roam-blog-index-context-fn index))
        (message "no context-fn defined for %s"
                 (org-roam-blog-index-title index))))))

(defsubst org-roam-blog-site--stage-index-entries (site index &optional entry-list)
  "Stage the individual entry pages the INDEX of a SITE.
ENTRY-LIST may come optionally pre-built."
  (if (and (org-roam-blog-index-entry-template index)
           (org-roam-blog-index-entry-context-fn index))
      (let ((template (org-roam-blog-index-entry-template index))
            (entry-context-list (org-roam-blog--build-entry-context-list index entry-list))
            (subdir
             (if-let ((index-slug (org-roam-blog-index-slug index))
                      (entry-dir (org-roam-blog-index-entry-dir index)))
                 (concat index-slug "/" entry-dir)
               index-slug)))
        (cl-loop for (node context) in entry-context-list
                 ;;FIXME: capture counters through `org-roam-blog--build-entry-context-list'
                 ;; for counter from 1 to (length entry-context-list)
                 do (org-roam-blog-stage
                     (funcall (org-roam-blog-index-entry-fname-fn index) node)
                     site template context subdir)))
    (block no-entry-pages-output
      (when (null (org-roam-blog-index-entry-template index))
        (message "no entry-template defined in %s"
                 (org-roam-blog-index-title index)))
      (when (null (org-roam-blog-index-entry-context-fn index))
        (message "no entry-context-fn defined in %s"
                 (org-roam-blog-index-title index))))))

(defsubst org-roam-blog-site--stage-index (site index)
  "Stage the INDEX of a SITE: process the feed view for entry groups
and individual page for each entry within an INDEX."
  (let ((entry-list (org-roam-blog--index-entry-list index)))
    (org-roam-blog-site--stage-index-groups  site index entry-list)
    (org-roam-blog-site--stage-index-entries site index entry-list)))

(defsubst org-roam-blog-site--process-registry (site)
  "Make all individual entry Id's point to their lead index
in the SITE's `entry-registry' hash table field."
  (cl-loop
   for index in (ht-values (org-roam-blog-site-index-ht site))
   do (when (org-roam-blog-index-leading index)
        (block process-registry
          (message "processing %s into global entry registry"
                   (org-roam-blog-index-title index))
          (let ((nodelist
                 (flatten-list (org-roam-blog--index-entry-list index))))
            (cl-loop
             for node in nodelist
             do (ht-set! (org-roam-blog-site-entry-registry site)
                         (org-roam-node-id node) index)))))))

(defsubst org-roam-blog-stage-site (site)
  "Main staging routine for a SITE. I'm using intermediate scratch temporary
directory and rsync (default for `org-roam-blog-local-sync-command'), in order
to also clean up orphans from the final `org-roam-blog-site-staging-dir'."
  ;; create and prepopulate scratch dir
  (setf (org-roam-blog-site-scratch-dir site)
        (make-temp-file "orb-" t))
  (shell-command
   (format "%s %s %s"
           org-roam-blog-local-sync-command
           (fname-with-tslash (org-roam-blog-site-src-root-dir site))
           (fname-no-tslash (org-roam-blog-site-scratch-dir site))))
  ;; generate global entry registry for the site
  (org-roam-blog-site--process-registry site)
  ;; output indexes contents for the site
  (let ((-orb--entry-registry (org-roam-blog-site-entry-registry site))) ; a bit of a hack here
    (cl-loop for index in (ht-values (org-roam-blog-site-index-ht site))
             do (block stage-index
                  (message "staging index %s" (org-roam-blog-index-title index))
                  (org-roam-blog-site--stage-index site index))))
  ;; sync with the staging directory
  (unless (f-exists? (org-roam-blog-site-staging-dir site))
    (f-mkdir (org-roam-blog-site-staging-dir site)))
  (shell-command
   (format "%s %s %s"
           org-roam-blog-local-sync-command
           (fname-with-tslash (org-roam-blog-site-scratch-dir site))
           (fname-no-tslash (org-roam-blog-site-staging-dir site))))
  ;; erase the intermediate scratch dir
  (f-delete (org-roam-blog-site-scratch-dir site) t)
  (setf (org-roam-blog-site-scratch-dir site) nil))

(defsubst org-roam-blog-start-preview (site)
  "Start `emacs-web-server' instance to serve the SITE staging directory content."
  (declare (type org-roam-blog-site site))
  (setq httpd-root (org-roam-blog-site-staging-dir site))
  (httpd-start))

;;;; Footer

(provide 'org-roam-blog-site)

;;; org-roam-blog-site.el ends here
