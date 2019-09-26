;;; module-company.el

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

(use-package company
  :ensure t
  :demand
  :delight (company-mode nil "company")
  :config
  (progn (setq company-idle-delay 0)
         (setq company-tooltip-limit 10)
         (setq company-echo-delay 0)
         (setq company-tooltip-flip-when-above t)
         (setq company-begin-commands '(self-insert-command))

         ;; Face Configuration
         (set-face-attribute 'company-tooltip-selection nil :background "#ddffdd")
         (set-face-attribute 'company-tooltip-selection nil :background "#cee7cf")
         (set-face-attribute 'company-tooltip-common nil :inherit 'fixed-pitch :foreground nil :underline nil :weight 'bold)
         (set-face-attribute 'company-scrollbar-fg nil :background "#388E3C")
         (set-face-attribute 'company-preview-common nil :underline nil)
         (set-face-attribute 'company-tooltip-common nil :underline nil)
         (set-face-attribute 'company-tooltip-common-selection nil :underline nil)))

(provide 'module-company)

;;; module-company.el ends here
