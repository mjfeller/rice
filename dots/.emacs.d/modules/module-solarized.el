;;; module-solarized.el --- solarized module for my emacs

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

(use-package solarized-theme
  :config
  (progn (setq solarized-emphasize-indicators nil
               solarized-high-contrast-mode-line nil
               solarized-scale-org-headlines nil
               solarized-use-less-bold t
               solarized-use-variable-pitch nil
               solarized-distinct-fringe-background nil)))

(use-package all-the-icons
  :demand
  :init
  (progn (defun -custom-modeline-github-vc ()
           (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
             (concat
              (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                          'face `(:height 1 :family ,(all-the-icons-octicon-family))
                          'display '(raise 0))
              (propertize (format " %s" branch))
              (propertize "  "))))

         (defun -custom-modeline-svn-vc ()
           (let ((revision (cadr (split-string vc-mode "-"))))
             (concat
              (propertize (format " %s" (all-the-icons-faicon "cloud"))
                          'face `(:height 1)
                          'display '(raise 0))
              (propertize (format " %s" revision) 'face `(:height 0.9)))))

         (defvar mode-line-my-vc
           '(:propertize
             (:eval (when vc-mode
                      (cond
                       ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
                       ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
                       (t (format "%s" vc-mode)))))
             face mode-line-directory)
           "Formats the current directory."))
  :config
  (progn
    ;; (setq-default mode-line-format
    ;;               (list
    ;;                evil-mode-line-tag
    ;;                mode-line-front-space
    ;;                mode-line-mule-info
    ;;                mode-line-modified
    ;;                mode-line-frame-identification
    ;;                mode-line-buffer-identification
    ;;                " "
    ;;                mode-line-position
    ;;                mode-line-my-vc
    ;;                mode-line-modes))
    ;; (concat evil-mode-line-tag)
    ))


;; (bind-keys ("C-c tl" . (lambda () (interactive) (load-theme 'solarized-light)))
;;            ("C-c td" . (lambda () (interactive) (load-theme 'solarized-dark))))

(load-theme 'solarized-light)

(set-face-attribute 'mode-line nil
                    :background "#eee8d5"
                    :foreground "#657b83"
                    :box '(:line-width 4 :color "#eee8d5")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#fdf6e3"
                    :foreground "#93a1a1"
                    :box '(:line-width 4 :color "#eee8d5")
                    :overline nil
                    :underline nil)

(define-minor-mode minor-mode-blackout-mode
  "Hides minor modes from the mode line."
  t)

(catch 'done
  (mapc (lambda (x)
          (when (and (consp x)
                     (equal (cadr x) '("" minor-mode-alist)))
            (let ((original (copy-sequence x)))
              (setcar x 'minor-mode-blackout-mode)
              (setcdr x (list "" original)))
            (throw 'done t)))
        mode-line-modes))

(global-set-key (kbd "C-c m") 'minor-mode-blackout-mode)
;; ;; window dividers
;; (window-divider-mode t)
;; (setq window-divider-default-right-width 2)

;; (set-face-attribute 'window-divider nil :foreground "#eee8d5")
;; (set-face-attribute 'window-divider-first-pixel nil :foreground "#eee8d5")
;; (set-face-attribute 'window-divider-last-pixel nil :foreground "#eee8d5")

(provide 'module-solarized)

;;; module-solarized.el ends here
