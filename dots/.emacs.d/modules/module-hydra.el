;; (use-package hydra
;;   :ensure t
;;   :bind (("C-c h"   . hydra-apropos/body)
;;          ("M-p"     . hydra-window/body)
;;          ("C-x SPC" . hydra-rectangle/body))
;;   :config
;;   (progn (defhydra hydra-goto-line (goto-map ""
;;                                              :pre (linum-mode 1)
;;                                              :post (linum-mode -1))
;;            "goto-line"
;;            ("g" goto-line "go")
;;            ("m" set-mark-command "mark" :bind nil)
;;            ("q" nil "quit"))

;;          (defhydra hydra-apropos (:color blue)
;;            "Apropos"
;;            ("m" man "man")
;;            ("a" apropos "apropos")
;;            ("c" apropos-command "cmd")
;;            ("d" apropos-documentation "doc")
;;            ("e" apropos-value "val")
;;            ("l" apropos-library "lib")
;;            ("o" apropos-user-option "option")
;;            ("u" apropos-user-option "option")
;;            ("v" apropos-variable "var")
;;            ("i" info-apropos "info")
;;            ("t" tags-apropos "tags")
;;            ("z" hydra-customize-apropos/body "customize"))

;;          (defhydra hydra-customize-apropos (:color blue)
;;            "Apropos (customize)"
;;            ("a" customize-apropos "apropos")
;;            ("f" customize-apropos-faces "faces")
;;            ("g" customize-apropos-groups "groups")
;;            ("o" customimze-apropos-options "options"))

;;          (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
;;                                               :color pink
;;                                               :hint nil
;;                                               :post (deactivate-mark))
;;            "
;;   ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
;; _h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
;;   ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
;; ^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
;; "
;;            ("k" rectangle-previous-line)
;;            ("j" rectangle-next-line)
;;            ("h" rectangle-backward-char)
;;            ("l" rectangle-forward-char)
;;            ("d" kill-rectangle)                    ;; C-x r k
;;            ("y" yank-rectangle)                    ;; C-x r y
;;            ("w" copy-rectangle-as-kill)            ;; C-x r M-w
;;            ("o" open-rectangle)                    ;; C-x r o
;;            ("t" string-rectangle)                  ;; C-x r t
;;            ("c" clear-rectangle)                   ;; C-x r c
;;            ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
;;            ("N" rectangle-number-lines)            ;; C-x r N
;;            ("r" (if (region-active-p)
;;                     (deactivate-mark)
;;                   (rectangle-mark-mode 1)))
;;            ("u" undo nil)
;;            ("g" nil))      ;; ok

;;          (defhydra hydra-window (:color red
;;                                         :columns nil)
;; ;;                       "
;; ;;   ^_k_^       _r_ horiz      _a_ balance      _s_ swap
;; ;; _h_   _l_     _v_ vertical   _t_ transpose    _q_ others
;; ;;   ^_j_^       _w_ kill       _u_ undo         _g_ quit"
;;            ("h" windmove-left)
;;            ("j" windmove-down)
;;            ("k" windmove-up)
;;            ("l" windmove-right)
;;            ("r" split-window-right)
;;            ("v" split-window-below)
;;            ("t" toggle-window-split)
;;            ("q" delete-other-windows :exit t)
;;            ("w" delete-window)
;;            ("a" balance-windows)
;;            ("s" ace-swap-window)
;;            ("d" ace-delete-window)
;;            ("g" nil)
;;            ("u" (progn (winner-undo) (setq this-command 'winner-undo))))))
