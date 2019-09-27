;;; core-packages.el --- Core packages for my Emacs config

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

;; (use-package paradox
;;   :config
;;   (progn (paradox-enable)
;;          (add-hook 'paradox-menu-mode-hook (lambda () (display-line-numbers-mode 0)))))

(use-package swiper
  :demand
  :delight (ivy-mode)
  :bind (("C-s"     . swiper)
         ("C-r"     . swiper)
         ("C-c u"   . swiper-all)
         ("C-c C-r" . ivy-resume)
         ("C-c C-o" . ivy-occur)
         ("C-c C-b" . ivy-switch-buffer)
         ("C-c C-k" . kill-buffer))
  :config
  (progn (ivy-mode 1)
         (setq ivy-height 6)
         (setq enable-recursive-minibuffers t)
         (setq swiper-include-line-number-in-search t)
         (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)))

;; ivy sorted via smex
(use-package smex
  :demand
  :config (setq smex-save-file (concat persistent-dir "/smex-items")))

(use-package counsel
  :after (ivy)
  :demand
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-c f r" . counsel-recentf)
         ("C-c g"   . counsel-git)
         ("C-c /"   . counsel-git-grep)
         ("M-y"     . counsel-yank-pop)))

(use-package saveplace
  :config
  (progn (setq save-place-file (concat persistent-dir "/places")
               backup-by-copying t
               delete-old-versions t
               kept-new-versions 6
               kept-old-versions 2
               version-control t)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("/ m"     . ibuffer-set-filter-groups-by-mode))
  :config
  (progn (setq ibuffer-default-sorting-mode 'major-mode
               ibuffer-display-summary nil)

         (defun ibuffer-get-major-modes-ibuff-rules-list (mm-list result-list)
           (if mm-list
               (let* ((cur-mm (car mm-list))
                      (next-res-list-el `(,(symbol-name cur-mm) (mode . ,cur-mm))))
                 (ibuffer-get-major-modes-ibuff-rules-list
                  (cdr mm-list) (cons next-res-list-el result-list)))
             result-list))

         (defun ibuffer-get-major-modes-list ()
           (mapcar
            (function (lambda (buffer)
                        (buffer-local-value 'major-mode (get-buffer buffer))))
            (buffer-list (selected-frame))))

         (defun ibuffer-create-buffs-group ()
           (interactive)
           (let* ((ignore-modes '(Buffer-menu-mode
                                  compilation-mode
                                  minibuffer-inactive-mode
                                  ibuffer-mode
                                  magit-process-mode
                                  messages-buffer-mode
                                  fundamental-mode
                                  completion-list-mode
                                  help-mode
                                  Info-mode))
                  (cur-bufs
                   (list (cons "Home"
                               (ibuffer-get-major-modes-ibuff-rules-list
                                (cl-set-difference
                                 (remove-duplicates
                                  (ibuffer-get-major-modes-list))
                                 ignore-modes) '())))))
             (setq ibuffer-saved-filter-groups cur-bufs)
             (ibuffer-switch-to-saved-filter-groups "Home")))

         (autoload 'ibuffer "ibuffer" "List buffers." t)

         (defun ibuffer-group-by-modes ()
           "Group buffers by modes."
           (ibuffer-create-buffs-group))

         (add-hook 'ibuffer-hook 'ibuffer-group-by-modes)

         (setq ibuffer-expert t)

         ;; Environment Variables
         (setq ibuffer-show-empty-filter-groups nil)
         (setq ibuffer-marked-char ?-)
         (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

         (defadvice ibuffer-update-title-and-summary (after remove-column-titles)
           (with-no-warnings
             (save-excursion
               (set-buffer "*Ibuffer*")
               (toggle-read-only 0)
               (goto-char 1)
               (search-forward "-\n" nil t)
               (delete-region 1 (point))
               ;; (let ((window-min-height 1))
               ;;   ;; save a little screen estate
               ;;   (shrink-window-if-larger-than-buffer))
               (toggle-read-only))))

         (ad-activate 'ibuffer-update-title-and-summary)

         ;; Use human readable Size column instead of original one
         (define-ibuffer-column size-h
           (:name "Size" :inline t)
           (cond
            ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
            ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
            ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
            (t (format "%8d" (buffer-size)))))

         ;; Modify the default ibuffer-formats
         (setq ibuffer-formats
               '((mark modified read-only " "
                       (name 18 18 :left :elide)
                       " "
                       (size-h 9 -1 :right)
                       " "
                       (mode 16 16 :left :elide)
                       " "
                       filename-and-process)))))

(use-package autopair
  :delight (autopair-mode)
  :config (autopair-global-mode))

(global-auto-revert-mode t)

(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))

(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
  :config
  (progn (setq wdired-use-dired-vertical-movement 'sometimes)

         (set-face-attribute 'dired-directory nil
                             :inherit 'default
                             :foreground "#839496"
                             :weight 'bold)

         (setq dired-listing-switches "-la")

         (defun dired-sort-dir-first ()
           "Sort dired listings with directories first."
           (save-excursion
             (let (buffer-read-only)
               (forward-line 2) ;; beyond dir. header
               (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
             (set-buffer-modified-p nil)))

         (defadvice dired-readin
             (after dired-after-updating-hook first () activate)
           "Sort dired listings with directories first before adding marks."
           (dired-sort-dir-first))))

;; better describe
(use-package helpful
  :bind (("C-h f" . helpful-function)
         ("C-h o" . helpful-symbol)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)))

(use-package rg
  :config
  (progn (rg-enable-default-bindings (kbd "M-s"))
         (setq rg-executable "/usr/local/bin/rg")))

;; window management
(bind-keys
 ("H-e" . windmove-up)
 ("H-d" . windmove-down)
 ("H-f" . windmove-right)
 ("H-s" . windmove-left)

 ;; Window Resizing
 ("H-E" . enlarge-window)
 ("H-D" . shrink-window)
 ("H-F" . enlarge-window-horizontally)
 ("H-S" . shrink-window-horizontally)

 ;; Window Splitting
 ("H-v" . split-window-vertically)
 ("H-r" . split-window-horizontally)
 ("H-w" . delete-window)
 ("H-q" . delete-other-windows)

 ;; Misc Window Commands
 ("H-a" . balance-windows)
 ("H-t" . toggle-window-split)
 ("H-<return>". toggle-fullscreen))

(use-package bind-chord
  :config
  (progn
    (key-chord-mode 1)

    (bind-chords
     ("wh" . windmove-left)
     ("wj" . windmove-down)
     ("wk" . windmove-up)
     ("wl" . windmove-right)

     ("wt" . split-window-horizontally)
     ("wv" . split-window-vertically)
     ("wq" . delete-window)
     ("wa" . balance-windows))

    (setq key-chord-two-key-delay 0.075)))

(delight 'subword-mode "" "subword")

;; woman
(add-hook 'woman-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'Man-mode-hook (lambda () (display-line-numbers-mode 0)))

(provide 'core-packages)

;;; core-packages.el ends here