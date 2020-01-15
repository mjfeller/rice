(setq shell-file-name "/bin/zsh")

(load-file (expand-file-name "theme/poet-fork/poet-theme.el" emacs-dir))
(load-theme 'poet)

(provide 'init-preload-local)
