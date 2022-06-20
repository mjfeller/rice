(use-package eglot
  :disabled
  :bind
  (:map eglot-mode-map
        ("C-c r" . eglot-rename)
        ("C-c o" . eglot-code-action-organize-imports)
        ("C-c h" . eldoc)
        ("M-."   . xref-show-definitions-function))
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'eglot--managed-mode (lambda () (flymake-mode -1))))

(use-package lsp-mode
  :config
  (setq lsp-keymap-prefix "H-l")
  (add-hook 'go-mode-hook #'lsp))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(provide 'module-lsp)
