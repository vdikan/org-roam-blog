;;; org-roam-blog.el --- ORB: a static site publishing facility for Org Roam  -*- lexical-binding: t; -*-

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

(require 'org-roam-blog-utils)
(require 'org-roam-blog-index)
(require 'org-roam-blog-site)

;; FIXME: this seems redundant. I can use the `site' instance for
;; global context, just carrying a global var pointing to it.
(cl-defstruct (org-roam-blog-global-context
               (:constructor org-roam-blog-global-context--create)
               (:copier nil))
  "Structure of `org-roam-blog-g' global context object for a
website being staged in the runtime."
  (site nil)
  (entry-registry nil))


(defvar org-roam-blog-g (org-roam-blog-global-context--create)
  "Global context object for a website being processed.")


(defsubst org-roam-blog-g-site-get ()
  "Get the website instance from the global context."
  (org-roam-blog-global-context-site org-roam-blog-g))


(defsubst org-roam-blog-g-site-set (obj)
  "Set the website instance from the global context to OBJ."
  (setf (org-roam-blog-global-context-site org-roam-blog-g)
        obj))


(defsubst org-roam-blog-g-entries-get ()
  "Get the website entry registry from the global context."
  (org-roam-blog-global-context-entry-registry org-roam-blog-g))


(defsubst org-roam-blog-g-entries-set (obj)
  "Set the website entry registry from the global context to OBJ."
  (setf (org-roam-blog-global-context-entry-registry org-roam-blog-g)
        obj))


(defsubst org-roam-blog-g-reset ()
  "Erases the Org Roam Blog global context."
  (org-roam-blog-g-site-set nil)
  (org-roam-blog-g-entries-set nil))

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

(defcustom org-roam-blog-anchor-regex "\\[\\[#[0-9a-zA-Z-]+\\]\\[§\\]\\]"
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
  (format " [[#%s][§]]" org-roam-blog-anchor-id-format)
  "Default formatter for the heading anchor."
  :group 'org-roam-blog
  :type 'string)

(defcustom org-roam-blog-outline-content-start-regexp ":END:.*\n"
  "Regex used to find the beginning of node's content."
  :group 'org-roam-blog
  :type 'string)

(defcustom org-roam-blog-dynmod-arch "x86_64"
  "Specifies architecture for which the rust dynamic module was built."
  :group 'org-roam-blog
  :type 'string)

;; Base path for the package
(defconst org-roam-blog-base (file-name-directory load-file-name))

;; the following loads my experimental Rust dynamic module:
(defun org-roam-blog-load-dynamic-module ()
  (module-load
   (expand-file-name (format "orb_dynmod_%s.so"
                             org-roam-blog-dynmod-arch)
                     org-roam-blog-base)))
;; a call of that function `my-org-dynmod/org-to-html' for
;; `org-roam-blog-utils'

;;;; Footer

(provide 'org-roam-blog)

;;; org-roam-blog.el ends here
