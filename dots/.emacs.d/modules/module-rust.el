;;; module-rust.el --- Packages used for rust

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

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c"   . compile)
              ("C-c <tab>" . rust-format-buffer))
  :config
  (progn (setq rust-format-on-save t)

         (defun rust-prog-init ()
             "if cargo is install get all depencies for emacs rust packages"
             (interactive)
             (if (not (eq (executable-find "cargo") nil))
                 (progn
                   (mapcar (lambda (pkg)
                             (let ((cmd (concat "cargo install " pkg)))
                               (call-process-shell-command cmd nil "*cargo-output*" t)))
                           '("racer")))
               (message "cargo executable not found")))

         (defun setup-rust-mode-compile ()
           "Customize compile command to run go build"
           (if (not (string-match "cargo" compile-command))
               (set (make-local-variable 'compile-command)
                    "cargo build")))

         (add-hook 'rust-mode-hook 'setup-rust-mode-compile)))


;; depends on racer install with cargo install racer
(use-package racer
  :after (rust-mode)
  :delight racer-mode
  :bind (:map rust-mode-map
              ("TAB"   . company-indent-or-complete-common)
              ("C-c d" . racer-describe))
  :config
  (progn (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
         (setq racer-rust-src-path (concat (getenv "HOME") "/prog/rust/src")) ;; Rust source code PATH

         (add-hook 'rust-mode-hook #'racer-mode)
         (add-hook 'racer-mode-hook #'eldoc-mode)
         (add-hook 'racer-mode-hook #'company-mode)

         (setq company-tooltip-align-annotations t)))

(use-package flycheck-rust
  :after (flycheck-mode rust-mode)
  :config
  (progn (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'module-rust)

;;; module-rust.el ends here
