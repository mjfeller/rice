;;; core-paths.el --- path confiugration for my emacs

;; Author: Mark Feller <mark.feller@member.fsf.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;; move all the misc files created by emacs into a seperate directory to
;; decluter emacs config root

;;; Code:

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq auto-save-list-file-prefix
      (concat persistent-dir "/auto-save-list/.saves-"))

(setq bookmark-default-file
      (concat persistent-dir "bookmarks.el"))

(setq ede-project-placeholder-cache-file
      (concat temporary-file-directory "/ede-projects.el"))

(setq semanticdb-default-save-directory
      (concat temporary-file-directory "/semanticdb"))

(setq abbrev-file-name
      (concat persistent-dir "/abbrev_defs.el"))

(setq tramp-persistency-file-name
      (concat persistent-dir "/tramp.el"))

(setq recentf-save-file
      (concat persistent-dir "/recentf"))

(setq org-id-locations-file
      (concat persistent-dir "/org-id-locations.el"))

(setq nsm-settings-file
      (concat persistent-dir "/network-security.data"))

(setq url-configuration-directory
      (concat persistent-dir "/url/"))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(provide 'core-paths)

;;; core-paths.el ends here
