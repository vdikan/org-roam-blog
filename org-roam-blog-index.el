;;; org-roam-blog-index.el --- ORB's Index definitions  -*- lexical-binding: t; -*-

(require 'org-roam-blog-utils)

(defvar org-roam-blog--index-root-speclist '(all node query)
  "Spec symbols for ORB Index types.")

(defun org-roam-blog--index-root-spec-p (thing)
  "Index Root has a form of cons cell:
  CAR is a spec symbol (for dispatch); rest is data for search."
  (and (consp thing)
       (member (car thing) org-roam-blog--index-root-speclist)))

(deftype org-roam-blog-index-root-spec-type ()
  '(satisfies org-roam-blog--index-root-spec-p))

(define-error 'org-roam-blog-index-root-error
   "ORB Index Error: Index Root should exist and be valid."
   'org-roam-blog-errors)

(define-error 'org-roam-blog-no-queries-error
   "ORB Error: Query Roots are not implemented yet."
   'org-roam-blog-errors)

(defun org-roam-blog--validate-root-spec (root-spec)
  (when (eql (car root-spec) 'query)
    (signal 'org-roam-blog-no-queries-error root-spec))

  (or (eql (car root-spec) 'all)
      (and (eql (car root-spec) 'node)
           (org-roam-node-from-id (cadr root-spec)))))

(defun org-roam-blog--slugify-default (string)
  "Indes slug-builder; defaults to calling `org-roam-blog-utils-slugify'."
  (funcall #'org-roam-blog-utils-slugify string))

(cl-defgeneric org-roam-blog--guess-index-title (root-spec)
  "Tries to guess a title for index based on its ROOT-SPEC.")

(cl-defmethod org-roam-blog--guess-index-title
  ((root-spec (head all)))
  "ORB Index Instance")

(cl-defmethod org-roam-blog--guess-index-title
 ((root-spec (head node)))
 (org-roam-node-title
  (org-roam-node-from-id (cadr root-spec))))

(defun org-roam-blog--filter-default (node)
  "Default node filtering function for index nodelist construction.
T unless NODE tagged win `noexport'."
  (not (member "noexport" (org-roam-node-tags node))))


(defun org-roam-blog--sort-default (node1 node2)
  "Default node sorting key for index nodelist construction.
Intended to leave nodes sorted in descending order w/r to date
they were added; compares `org-timestamp' date field in the
\"ADDED\" property record of the nodes.

T when NODE1 appears added later than NODE2, or if any of the
nodes are missing \"ADDED\" property (leaving them in place)."
  (let ((ds1 (cdr (assoc "ADDED" (org-roam-node-properties node1))))
        (ds2 (cdr (assoc "ADDED" (org-roam-node-properties node2)))))
    (or (null ds1)
        (null ds2)
        (not (calendar-date-compare     ; that thing needs lists of date-lists :/
              (list (org-roam-blog-to-calendar-date ds1))
              (list (org-roam-blog-to-calendar-date ds2)))))))

(cl-defstruct (org-roam-blog-index (:constructor org-roam-blog-index--create)
                                   (:copier nil))
  (root-spec        nil  :type org-roam-blog-index-root-spec-type)
  (title            nil  :type string)
  (slug             nil  :type string)

  (staging-dir      nil  :type string)
  (media-dir        nil  :type string)

  (template         nil  :type string)
  (entry-template   nil  :type string)

  (slug-fn   #'org-roam-blog--slugify-default
             :type function)
  (filter-fn #'org-roam-blog--filter-default
             :type function)
  (sort-fn   #'org-roam-blog--sort-default
             :type function)
  (group-by  nil :type integer)

  (context-fn       nil  :type function)
  (entry-context-fn nil  :type function))

(cl-defun org-roam-blog-index-create (&rest args)
  (condition-case err
      (let* ((index (apply #'org-roam-blog-index--create args))
             (root-spec (org-roam-blog-index-root-spec index)))

        (unless (and root-spec (org-roam-blog--validate-root-spec root-spec))
          (signal 'org-roam-blog-index-root-error root-spec))

        (unless (org-roam-blog-index-title index)
          (setf (org-roam-blog-index-title index)
                (org-roam-blog--guess-index-title root-spec)))

        (unless (org-roam-blog-index-slug index)
          (setf (org-roam-blog-index-slug index)
                (funcall (org-roam-blog-index-slug-fn index)
                         (org-roam-blog-index-title index))))
        index)
    (org-roam-blog-errors
     (message "%s" (error-message-string err))
     nil)))

(cl-defgeneric org-roam-blog--entry-list-pre-builder (root-spec)
    "Initial entry list retriever based on the ROOT-SPEC.")

  (cl-defmethod org-roam-blog--entry-list-pre-builder
    ((root-spec (head all)))
    "Just returns `org-roam-node-list' function symbol to get all Roam's nodes."
    #'org-roam-node-list)

  (cl-defmethod org-roam-blog--entry-list-pre-builder
    ((root-spec (head node)))
    "Returned lambda gets entries' subheadings of the node's heading
as Org-Roam nodes (generate and save their Id's when needed).
The NODE ROOT-SPEC can specify entry heading offset other than 1
via :ENTRY-OFFSET keyword property."
    (lambda ()
      (with-current-buffer
          (org-roam-node-find-noselect
           (org-roam-node-from-id (cadr root-spec)) t)
        (remove-if #'null
                   (let ((root-level (org-current-level))
                         (entry-offset (or (getf root-spec :entry-offset) 1)))
                     (org-map-entries
                      (lambda ()
                        (when (= root-level
                                 (- (org-current-level) entry-offset))
                          (org-id-get-create)
                          (when (buffer-modified-p)
                            (save-buffer)
                            (org-roam-db-update-file))
                          (org-roam-node-at-point)))
                      nil 'tree))))))

(defun org-roam-blog--index-entry-list (index)
  "Return filtered list of Roam entries to be published for INDEX."
  (let* ((filter-fn (org-roam-blog-index-filter-fn index))
         (sort-fn (org-roam-blog-index-sort-fn index))
         (group-by (org-roam-blog-index-group-by index))
         (nodes (funcall (org-roam-blog--entry-list-pre-builder
                          (org-roam-blog-index-root-spec index))))
         (nodes (cl-remove-if-not (lambda (n)
                                    (if filter-fn (funcall filter-fn n) t)) nodes))
         (_ (when sort-fn (setq nodes (seq-sort sort-fn nodes)))))
    (if (integerp group-by)
        (if (zerop group-by)
            (list nodes)
          (seq-partition nodes group-by))
      nodes)))

;;;; Footer

(provide 'org-roam-blog-index)

;;; org-roam-blog-index.el ends here
