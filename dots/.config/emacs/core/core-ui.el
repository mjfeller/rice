;;; core-ui.el --- ui configuration for my emacs

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

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(blink-cursor-mode -1)            ; the blinking cursor is nothing, but an annoyance
(setq ring-bell-function 'ignore) ; disable the annoying bell ring
(setq inhibit-startup-screen t)   ; disable startup screen
(fset 'yes-or-no-p 'y-or-n-p)     ; enable y/n answers

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line numbers
(line-number-mode 0)
(column-number-mode t)

(delete-selection-mode t) ; Delete Marked regions
(show-paren-mode t)       ; Show matching parenthesis

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default indicate-empty-lines nil)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq-default fill-column 72)

(add-hook 'before-save-hook 'delete-tailing-whitespace)

(setq echo-keystrokes 0.1)

(setq frame-resize-pixelwise t)

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

;; line numbers
(setq-default display-line-numbers-width 3)
(setq display-line-numbers-type 'relative
      display-line-numbers-current-absolute t
      display-line-numbers-widen nil) ; don't count narrowed regions

(setq-default line-spacing 5)

;; add some padding around the entire frame and fringes
(if (eq system-type 'darwin)
    (set-fringe-mode 0)
  (set-fringe-mode 25))
(set-frame-parameter nil 'internal-border-width 0)

(provide 'core-ui)

;;; core-ui.el ends here
