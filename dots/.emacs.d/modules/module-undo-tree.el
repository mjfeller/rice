(delight 'undo-tree-mode "" "undo-tree")

(use-package undo-tree
  :disabled
  :delight (undo-tree-mode "" "undo-tree")
  :config (global-undo-tree-mode t))

(provide 'module-undo-tree)
