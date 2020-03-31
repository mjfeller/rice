;;; module-git.el

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

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix))
  :init
  (progn (defun magit-status-with-prefix ()
           (interactive)
           (let ((current-prefix-arg '(4)))
             (call-interactively 'magit-status)))

         (setq transient-history-file (concat persistent-dir "/transient/history.el")
               transient-display-buffer-action '(display-buffer-below-selected)
               transient-mode-line-format
               '("%e" mode-line-front-space mode-line-buffer-identification))))

;; (use-package evil-magit)

(use-package gitignore-mode)

(use-package magithub
  :disabled
  :after magit
  :config
  (progn (magithub-feature-autoinject t)
         (setq magithub-api-timeout 5
               magithub-dir (concat persistent-dir "/magithub"))))

(use-package git-timemachine)

(use-package git-gutter-fringe+
  :disabled
  :delight git-gutter+-mode
  :config
  (progn (require 'git-gutter-fringe+)
         (git-gutter-fr+-minimal)
         (global-git-gutter+-mode)))

(provide 'module-git)

;;; module-git.el ends here
