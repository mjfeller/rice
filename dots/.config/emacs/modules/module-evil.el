;;; module-evil.el

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

(use-package evil
  :config
  (progn (setq evil-mode-line-format '(before . mode-line-front-space))
         (setq evil-echo-state nil)

         ;; prevent esc-key from translating to meta-key in terminal mode
         (setq evil-esc-delay 0)

         (evil-mode 1)

         (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
         (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
         (define-key evil-insert-state-map (kbd "C-u")
           (lambda ()
             (interactive)
             (evil-delete (point-at-bol) (point))))))

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;; evil-goggles will slightly delay vi operations and show a shadow on the text
;; area being operated on. Extremely useful for pair programming
(use-package evil-goggles
  :delight (evil-goggles-mode)
  :config
  (progn (evil-goggles-mode t)
         (setq evil-goggles-pulse t)
         (setq evil-goggles-blocking-duration 0.100)
         (setq evil-goggles-async-duration 0.300)))

(use-package evil-surround
  :config (global-evil-surround-mode t))

(use-package evil-commentary
  :disabled
  :delight (evil-commentary-mode)
  :config (evil-commentary-mode t))

(use-package evil-snipe
  :disabled
  :delight (evil-snipe-mode nil "snipe")
  :config (evil-snipe-override-mode t))

(use-package evil-org
  :disabled
  :delight (evil-org-mode)
  :config
  (progn (add-hook 'org-mode-hook 'evil-org-mode)

         (require 'evil-org-agenda)
         (evil-org-agenda-set-keys)
         (evil-org-set-key-theme '(textobjects
                                   insert
                                   navigation
                                   additional
                                   shift
                                   todo
                                   heading))))

(provide 'module-evil)

;;; module-evil.el ends here
