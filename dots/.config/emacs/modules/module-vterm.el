(use-package vterm
  :config
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (add-to-list 'evil-emacs-state-modes 'compilation-mode)
  (add-to-list 'vterm-mode-hook 'toggle-modeline))

(use-package multi-vterm
  :bind (("C-x p t" . multi-vterm-project)
         ("H-T" . multi-vterm-dedicated-open)
         ("H-u" . multi-vterm-next)
         ("H-i" . multi-vterm-prev)
         ("H-y" . multi-vterm)))

(provide 'module-vterm)
