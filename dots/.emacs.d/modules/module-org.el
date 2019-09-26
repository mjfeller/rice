;;; module-org.el

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

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-src-mode-map
         ("C-x C-s" . org-edit-src-exit))
  :delight (org-indent-mode nil t)
  :config
  (progn (require 'org-indent)
         (require 'org-src)

         (setq org-startup-indented t)
         (setq org-src-tab-acts-natively t)
         (setq org-src-fontify-natively t)
         (setq org-hide-emphasis-markers t)

         (defun org-summary-todo (n-done n-not-done)
           "Switch entry to DONE when all subentries are done, to TODO otherwise."
           (let (org-log-done org-log-states)   ; turn off logging
             (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
         (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

         ;; disable line numbers in org mode
         (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

         ;; where I keep all my org files
         (setq org-agenda-files
               '("~/org/work.org"
                 "~/org/life.org"))

         ;; personal capture templates
         (setq org-capture-templates
               '(
                 ;; unfiled work items
                 ("w" "Work" entry (file+headline "~/org/work.org" "Unread")
                  "* TODO %?\n  %i\n")

                 ;; random thoughts I'd like to capture
                 ("t" "Thought" entry (file+headline "~/org/work.org" "Thoughts")
                  "* %?\n  %i\n")

                 ;; unfiled work items in my life
                 ("l" "Life" entry (file+headline "~/org/life.org" "Inbox")
                  "* TODO %?\n  %i\n")

                 ("j" "Journal" entry (file+datetree "~/org/life.org")
                  "* TODO %?\n")))

         ;; when refiling an org header don't search more than 3 levels deep
         (setq org-refile-targets
               '((nil :maxlevel . 3)
                 (org-agenda-files :maxlevel . 3)))

         (setq org-todo-keywords
               '((sequence "TODO(t)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")
                 (sequence "RECURRING(r)" "|" "DONE(d)")))

         ;; don't make the title big, I don't like that. Also remove those ugly
         ;; checkboxes
         (set-face-attribute 'org-document-title nil :height 1)
         (set-face-attribute 'org-checkbox nil :box nil)))

(use-package org-bullets
  :config
  (progn (setq org-bullets-bullet-list '("●" "○" "◉" "•"))

         ;; bullets on lists
         (font-lock-add-keywords
          'org-mode
          '(("^ +\\([-*]\\) "
             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))))

(use-package org-fancy-priorities
  :ensure t
  :delight (org-fancy-priorities-mode)
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (progn (setq org-fancy-priorities-list '("⬆" "➡" "⬇" "☕"))

          ;; solarized theme on priorities
          (setq org-priority-faces '((65 . "#dc322f")
                                     (66 . "#cb4b16")
                                     (67 . "#859900")))))

(use-package deft
  :disabled
  :config
  (progn (setq deft-directory "~/org"
               deft-extensions '("md" "org")
               deft-recursive t)

         (add-hook 'deft-mode-hook (lambda () (evil-emacs-state)))))

(provide 'module-org)

;;; module-org.el ends here
