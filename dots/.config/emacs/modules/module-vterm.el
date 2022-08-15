(use-package vterm
  :config
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (add-to-list 'evil-emacs-state-modes 'compilation-mode)
  (add-to-list 'vterm-mode-hook 'toggle-modeline))

(use-package multi-vterm
  :bind (("C-x p t" . multi-vterm-project)
         ((concat window-management-prefix "-T") . multi-vterm-dedicated-open)
         ((concat window-management-prefix "-u") . multi-vterm-next)
         ((concat window-management-prefix "-i") . multi-vterm-prev)
         ((concat window-management-prefix "-y") . multi-vterm)))

(provide 'module-vterm)
