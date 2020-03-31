;;; module-projectile.el

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

;;; Code:

(use-package projectile
  :delight (projectile-mode)
  :config
  (progn (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
         (setq projectile-completion-system 'ivy
               projectile-cache-file (expand-file-name "projectile-cache" persistent-dir)
               projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" persistent-dir))))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(provide 'module-projectile)

;;; module-projectile.el ends here
