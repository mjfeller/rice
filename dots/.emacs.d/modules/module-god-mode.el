;;; module-god-mode.el

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

(use-package god-mode
  :bind (("<escape>" . god-mode-all)
         :map god-local-mode-map
         ("z" . repeat)
         ("i" . god-mode-all))
  :config
  (progn (defun my-update-cursor ()
           (setq cursor-type (if (or god-local-mode buffer-read-only)
                                 'box
                               'bar)))

         (add-hook 'god-mode-enabled-hook 'my-update-cursor)
         (add-hook 'god-mode-disabled-hook 'my-update-cursor)))

(provide 'module-god-mode)

;;; module-god-mode.el ends here

