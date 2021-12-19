;;; org-roam-blog.el --- ORB: a static site publishing facility for Org Roam  -*- lexical-binding: t; -*-

(require 'org-roam-blog-utils)
(require 'org-roam-blog-index)
(require 'org-roam-blog-site)

(defcustom org-roam-blog-default-date-property "ADDED"
  "Default header property of the Org item headers used for sorting in Indexes."
  :group 'org-roam-blog
  :type  'string)


(defcustom org-roam-blog-default-entry-dir-name "entry"
  "Name of default subdirectory containing entries for an index."
  :group 'org-roam-blog
  :type  'string)


(defcustom org-roam-blog-default-media-dir-name "media"
  "Name of default subdirectory containing media files for an index."
  :group 'org-roam-blog
  :type  'string)


(defcustom org-roam-blog-index-filename-prefix "index"
  "Name of default filename prefix for an index."
  :group 'org-roam-blog
  :type  'string)


(defcustom org-roam-blog-entry-filename-prefix "entry"
  "Name of default filename prefix for an entry."
  :group 'org-roam-blog
  :type  'string)

;;;; Footer

(provide 'org-roam-blog)

;;; org-roam-blog.el ends here
