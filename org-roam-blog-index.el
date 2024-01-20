;;; org-roam-blog-index.el --- ORB's Index definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Vladimir Dikan

;; Author: Vladimir Dikan <vdikan@vivaldi.net>
;; Version: 0.0.1
;; Keywords: org-mode, org-roam, blog, extensions, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'ht)
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
  "Indes slug-builder; defaults to calling `org-roam-blog-slugify'."
  (funcall #'org-roam-blog-slugify string))

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
property set as `org-roam-blog-default-date-property' record of
the nodes.

T when NODE1 appears added later than NODE2, or if any of the
nodes are missing `org-roam-blog-default-date-property' property
(leaving them in place)."
  (let ((ds1 (cdr (assoc org-roam-blog-default-date-property
                         (org-roam-node-properties node1))))
        (ds2 (cdr (assoc org-roam-blog-default-date-property
                         (org-roam-node-properties node2)))))
    (or (null ds1)
        (null ds2)
        (not (calendar-date-compare     ; that thing needs lists of date-lists :/
              (list (org-roam-blog-to-calendar-date ds1))
              (list (org-roam-blog-to-calendar-date ds2)))))))

(defun org-roam-blog--entry-context-default (node)
  "Default NODE to context hash-table processor."
  (let* ((lead-index (org-roam-node--lead-index-for (org-roam-node-id node)))
         (backlinks (org-roam-blog--backlinks-to-context node))
         (date (cdr (assoc org-roam-blog-default-date-property
                          (org-roam-node-properties node))))
         (tags (mapcar (lambda (tag) (ht ("tag" tag)))
                      (org-roam-node-tags node)))
         (toclevel (cdr (assoc org-roam-blog-toc-level-property
                         (org-roam-node-properties node))))
         (toc (when toclevel (org-roam-blog--toc-to-context
                              node (string-to-number toclevel)))))
    (ht ("title" (org-roam-node-title node))
        ("lead-index-title" (org-roam-blog-index-title lead-index))
        ("backlinks" backlinks)
        ("show-backlinks" (when backlinks t))
        ("toc" toc)
        ("show-toc" (when toc t))
        ("date" date)
        ("show-date" (when date t))
        ("tags" tags)
        ("show-tags" (when tags t))
        ("self-url" (org-roam-blog--relative-entry-url node lead-index))
        ("main" (org-roam-blog--htmlize-node-content node)))))

(defun org-roam-blog--entry-fname-default (node)
  "Default filename builder for entry NODE object."
  (->> (org-roam-node-title node)
       (org-roam-blog-slugify)
       (format "%s.html")))

(defsubst org-roam-blog--entry-fname-with-date (node)
  "Alternative filename builder NODE object that prepends the entry date."
  (cl-labels
      ((datestring-to-slug-part
        (datestring)
        (let ((date (org-roam-blog-to-calendar-date datestring)))
          (cl-destructuring-bind (month day year) date
            (format "%4d-%02d-%02d-" year month day)))))
    (let ((datestring
           (cdr (assoc org-roam-blog-default-date-property
                       (org-roam-node-properties node)))))
      (concatenate 'string
                   (datestring-to-slug-part datestring)
                   (org-roam-blog--entry-fname-default node)))))

(cl-defstruct (org-roam-blog-index (:constructor org-roam-blog-index--create)
                                   (:copier nil))
  (root-spec        nil  :type org-roam-blog-index-root-spec-type)
  (title            nil  :type string)
  (slug             nil  :type string)

  (entry-dir  org-roam-blog-default-entry-dir-name :type string)
  (media-dir  org-roam-blog-default-media-dir-name :type string)

  (template         nil  :type string)
  (entry-template   nil  :type string)

  (slug-fn   #'org-roam-blog--slugify-default
             :type function)
  (filter-fn #'org-roam-blog--filter-default
             :type function)
  (sort-fn   #'org-roam-blog--sort-default
             :type function)
  (group-by  0 :type integer)

  (context-fn  #'org-roam-blog--index-context-default
               :type function)
  (entry-context-fn #'org-roam-blog--entry-context-default
                    :type function)
  (entry-fname-fn #'org-roam-blog--entry-fname-default
                  :type function)
  (leading   t :type boolean))

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
    (if (zerop group-by)
        (list nodes)
      (seq-partition nodes group-by))))

(defsubst org-roam-blog--index-context-default (index &optional entry-list)
  "Sample constructor of a context for INDEX.
Set in \"context-fn\" field by default.
Optionally accepts pre-built ENTRY-LIST."
  (let* ((title (org-roam-blog-index-title index))
         (slug  (org-roam-blog-index-slug index))
         (entry-dir  (org-roam-blog-index-entry-dir index))
         (media-dir  (org-roam-blog-index-media-dir index))
         (entry-list (or entry-list (org-roam-blog--index-entry-list index)))
         (page-max (length entry-list)))
    (cl-loop for page-num from 1
             for entry-group in entry-list
             collect (let ((entry-group
                            (mapcar (org-roam-blog-index-entry-context-fn index)
                                    entry-group))
                           (prev-page
                            (if (> page-num 1)
                              (format "/%s/%s-%s.html"
                                      slug
                                      org-roam-blog-index-filename-prefix
                                      (1- page-num))

                              ""))      ; must result in a string to render in mustache
                           (next-page
                            (if (< page-num page-max)
                              (format "/%s/%s-%s.html"
                                      slug
                                      org-roam-blog-index-filename-prefix
                                      (1+ page-num))
                              "")))     ; must result in a string to render in mustache
                       (ht ("title" title)
                           ("slug" slug)
                           ("entry-dir" entry-dir)
                           ("media-dir" media-dir)
                           ("entries" entry-group)
                           ("page" page-num)
                           ("page-max" page-max)
                           ("prev-page" prev-page)
                           ("has-prev-page" (not (string-empty-p prev-page)))
                           ("next-page" next-page)
                           ("has-next-page" (not (string-empty-p next-page))))))))

(defsubst org-roam-blog--build-entry-context-list (index &optional entry-list)
    "Prototype function to get a list of entry contexts for INDEX publishing.
Optionally accepts pre-built grouped ENTRY-LIST, meant be flattened."
    (cl-loop for node in (flatten-list
                          (or entry-list
                              (org-roam-blog--index-entry-list index)))
             collect (list node
                           (funcall (org-roam-blog-index-entry-context-fn index) node))))

(defsubst org-roam-blog--subdir-for-index (index)
  "Return relative subdirectory for entry outputs for the INDEX
inside the staging directory."
  (if-let ((index-slug (org-roam-blog-index-slug index))
           (entry-dir (org-roam-blog-index-entry-dir index)))
        (concat index-slug "/" entry-dir)
      index-slug))

(defsubst org-roam-blog--relative-entry-url (node index)
  "Output relative entry url path for an entry defined by NODE and
an INDEX, implying that this INDEX is a leading one for the NODE."
  (let ((fname
         (funcall (org-roam-blog-index-entry-fname-fn index) node))
        (subdir (org-roam-blog--subdir-for-index index)))
    (string-trim (concat "/" subdir "/" fname))))

;;;; Footer

(provide 'org-roam-blog-index)

;;; org-roam-blog-index.el ends here
