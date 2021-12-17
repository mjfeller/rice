(setq shell-file-name "/bin/zsh")

(load-file (expand-file-name "lisp/acme/acme-theme.el" emacs-dir))
(load-theme 'acme)

(provide 'init-preload-local)
