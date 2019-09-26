;;; module-neotree.el

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

(use-package neotree
  :config
  (progn (setq neo-theme 'arrow
               neo-show-slash-for-folder nil)

         ;; disable modeline and line numbers
         (add-hook 'neotree-mode-hook 'toggle-modeline)
         (add-hook 'neotree-mode-hook (lambda () (display-line-numbers-mode 0)))

          (set-face-attribute 'neo-dir-link-face nil :inherit 'dired-directory)
          (set-face-attribute 'neo-file-link-face nil :inherit 'fixed-width)
          (set-face-attribute 'neo-header-face nil :inherit 'fixed-width :weight 'bold)
          (set-face-attribute 'neo-root-dir-face nil :inherit 'fixed-width :weight 'bold)))

(provide 'module-neotree)

;;; module-neotree.el ends here
