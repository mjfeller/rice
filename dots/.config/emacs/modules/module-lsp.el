(use-package lsp-mode
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)


(add-hook 'go-mode-hook #'lsp)
(setq lsp-eldoc-render-all nil)
