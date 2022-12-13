;;; org-roam-blog.el --- ORB: a static site publishing facility for Org Roam  -*- lexical-binding: t; -*-

(require 'org-roam-blog-utils)
(require 'org-roam-blog-index)
(require 'org-roam-blog-site)

(defvar -orb--site nil
  "Global context object for meta of a site being exported.")

(defvar -orb--entry-registry nil
  "A ref to the entry registry of a site being exported.")

(defcustom org-roam-blog-local-sync-command "rsync -a --delete"
  "Shell command for local folder synchronization during site export."
  :group 'org-roam-blog
  :type  'string)


(defcustom org-roam-blog-html-fn-property "ORB_HTML_FN"
  "Header property of designating alternative HTMLizer functions."
  :group 'org-roam-blog
  :type  'string)


(defcustom org-roam-blog-html-src-property "ORB_HTML_SRC"
  "Header property pointing to an alternative pregenerated HTML markup for a node."
  :group 'org-roam-blog
  :type  'string)


(defcustom org-roam-blog-toc-level-property "TOC_LEVEL"
  "TOC level header property. Headlines of this level or higher will appear in contents."
  :group 'org-roam-blog
  :type  'string)


(defcustom org-roam-blog-default-date-property "ADDED"
  "Default header property of the Org item headers used for sorting in Indexes."
  :group 'org-roam-blog
  :type  'string)


(defcustom org-roam-blog-default-entry-dir-name "items"
  "Name of default subdirectory containing entries for an index."
  :group 'org-roam-blog
  :type  'string)


(defcustom org-roam-blog-index-filename-prefix "index"
  "Name of default filename prefix for an index."
  :group 'org-roam-blog
  :type  'string)

(defcustom org-roam-blog-default-media-dir-name "media"
  "Name of default subdirectory containing media files for an index."
  :group 'org-roam-blog
  :type  'string)


(defcustom org-roam-blog-media-image-extensions '("png" "jpg" "jpeg" "gif" "svg")
  "List of extensions for image files for media exporting."
  :group 'org-roam-blog
  :type  'list)


;; FIXME: this setting should be site-customizeable
(defcustom org-roam-blog-media-image-inline-format-string 
  "#+begin_export html\n<figure class=\"image\">\n<img class=\"lightense\" src=\"%s\">\n</figure>\n#+end_export"
  "Format string that will envelop media image file URL link."
  :group 'org-roam-blog
  :type  'string)

(defcustom org-roam-blog-anchor-regex "\\[\\[#[0-9a-zA-Z-]+\\]\\[¶\\]\\]"
  "Regex used to find anchor link in a title of a headline."
  :group 'org-roam-blog
  :type 'string)


(defcustom org-roam-blog-sure-headline-regex "^*+[ ]+"
  "Regex used to ensure the headline is true and not just a bold text
  (that would not include interna whitespace)."
  :group 'org-roam-blog
  :type 'string)

(defvar org-roam-blog-anchor-id-format "%s-%s"
  "title-idx. Just to be sure it is unnified.")


(defcustom org-roam-blog-anchor-format
  (format " [[#%s][¶]]" org-roam-blog-anchor-id-format)
  "Default formatter for the heading anchor."
  :group 'org-roam-blog
  :type 'string)

(defcustom org-roam-blog-outline-content-start-regexp ":END:.*\n"
  "Regex used to find the beginning of node's content."
  :group 'org-roam-blog
  :type 'string)

;;;; Footer

(provide 'org-roam-blog)

;;; org-roam-blog.el ends here
