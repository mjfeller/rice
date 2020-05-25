;;; poet-theme.el --- A theme for prose.

;; Copyright 2018-now Kunal Bhalla

;; Author: Kunal Bhalla <bhalla.kunal@gmail.com>
;; URL: https://github.com/kunalb/poet/
;; Version: 2.0

;;; Commentary:

;; Emacs has very good support for multiple fonts in a single
;; file.  Poet uses this support to make it much more convenient to
;; write prose within Emacs, with particular attention paid to
;; org-mode and markdown-mode.  Code blocks, tables, etc are
;; formatted in monospace text with the appropriate backgrounds.

;; Recommended customizations for using this theme
;;
;; - Set up the base fonts you'd like to use in Emacs before loading Poet
;;     (set-face-attribute 'default nil :family "Iosevka" :height 130)
;;     (set-face-attribute 'fixed-pitch nil :family "Iosevka")
;;     (set-face-attribute 'variable-pitch nil :family "Baskerville")
;;   On loading this theme captures the default and treats that for fixed-pitch
;;   rendering.
;;
;; - Enable variable pitch mode for editing text
;; (add-hook 'text-mode-hook
;;            (lambda ()
;;             (variable-pitch-mode 1))
;;
;; - Some other modes I like to enable/disable
;;     (olivetti-mode 1)        ;; Centers text in the buffer
;;     (flyspell-mode 1)        ;; Catch Spelling mistakes
;;     (typo-mode 1)            ;; Good for symbols like em-dash
;;     (blink-cursor-mode 0)    ;; Reduce visual noise
;;     (linum-mode 0)           ;; No line numbers for prose
;;
;; - And prettier org mode bullets:
;;     (setq org-bullets-bullet-list
;;         '("◉" "○"))
;;     (org-bullets 1)

;;; Code:

;; (set-face-attribute 'default nil :family "Roboto Mono")
;; (set-face-attribute 'default nil :family "Fira Mono")
;; (set-face-attribute 'default nil :family "Iosevka Slab")
(set-face-attribute 'default nil :family "Hack")

(defvar poet--monospace-height
 (face-attribute 'fixed-pitch :height nil 'default)
 "The original height stored as a defvar to stay constant across reloads.")

(defun poet--height (multiplier)
 "Scale up the height according to the MULTIPLIER."
 (truncate (* poet--monospace-height multiplier)))

(deftheme poet
  "A prose friendly theme.")

(make-face 'mode-line-height)
(set-face-attribute 'mode-line-height nil
    :foreground "#8888cc" :background "#8888cc" 
    :inverse-video nil
    :box '(:line-width 6 :color "#8888cc" :style nil))

(let (
      ;; Primary Colors
      (fg "#444444")
      ;; (bg "#e1d9c2")
      ;; (bg "#f6f6f6")
      (bg "#FFFFea")

      (modeline "#EAFFFF")

      (white-light  "#ffffff")
      (yellow-light "#fff59d")
      (red-light    "#ff1744")

      (black  "gray0")
      (white  "gray88")
      (yellow "#fff176")
      (blue   "#0288D1")
      (green  "#388E3C")
      (red    "#bf360c")
      (purple "#673AB7")
      (brown  "#8D6E63")

      ;; dark colors
      (cyan-dark   "#455A64")
      (blue-dark   "#3f51b5")
      (red-dark    "#8E3C38")
      (purple-dark "#6C3082")
      (brown-dark  "#4e342e")
      )
  (let (
        (emph "gray13")
        ;; (sep "gray90")
        (sep "gray50")
        (hlt "gray93")
        ;; (bg-hlt "#fff8e1")
        (bg-hlt "#e4e4e4")
        (meta brown-dark)
        (link "#303f9f")
        (link-underline "#304ffe")
        (vlink-underline "#1a237e")
        (button "gray27")
        (cursor "gray29")
        (paren-match-bg red-light)
        (paren-match-fg white-light)
        (search-fg red)
        (search-bg white-light)
        (search-fail-bg "#f8bbd0")
        (tooltip-fg "#111111")
        (tooltip-bg yellow)
        (shadow "grey60")
        (secondary-bg yellow-light)
        (trailing-bg "#ff8a65")
        (fci "grey87")
        (mode-line-fg "grey7")
        (mode-line-hlt white-light)
        (mode-line-inactive "#888888")
        (header red-dark)
        (header-line-bg white)
        (builtin red-dark)
        (string green)
        (function-name "#444444")
        (keyword "#444444")
        (constant blue)
        (type "#444444")
        (variable cyan-dark)

        ;; org
        (org-meta                brown)
        (org-document-info       brown)
        (org-table               white)
        (org-quote-fg            purple-dark)
        (org-quote-bg            white)
        (org-date                "#444444")
        (org-title               red)
        (org-title-underline     "#aaaaaa")
        (org-checkbox            "#aaaaaa")
        (org-scheduled           "#333333")
        (org-scheduled-today     "#111111")
        (org-done                green)
        (org-todo                red)
        (org-tag                 "#777777")
        (org-block-line          "#d0d0d0")
        (org-block-bg            white)
        (org-agenda-structure-fg "#555555")
        (org-agenda-structure-bg white)
        (org-agenda-today-fg     "#000000")
        (org-agenda-today-bg     "#eeeeee")
        (org-special-keyword     "#777777")
        (org-sched-prev          "#222222")
        (org-agenda-done         "#777777")

        (hl-line "#efefef")
        (linum-hlt "#555555")
        (linum "#aaaaaa")
        (markdown-markup brown)
        (markdown-metadata "#777777")
        (markdown-language purple)
        (markdown-list "#000000")
        (markdown-code-bg white)
        (markdown-pre-bg white)
        (markdown-header-delimiter brown)
        (imenu brown-dark))
    (custom-theme-set-faces 'poet
                            `(variable-pitch ((t (:family ,(face-attribute 'variable-pitch :family) :height ,(poet--height 1)))))

                            `(default ((t (:background ,bg :foreground ,fg))))
                            `(italic ((t (:foreground ,emph :slant italic))))
                            `(highlight ((t (:background ,hlt :overline nil))))
                            `(region ((t (:background ,bg-hlt))))
                            `(fringe ((t (:background ,bg))))
                            `(button ((t (:inherit default :foreground ,button))))
                            `(escape-glyph ((t (:foreground ,purple))))
                            `(link ((t (:underline (:color ,link-underline :style line) :foreground ,link))))
                            `(link-visited ((t (:inherit link :foreground ,link :underline (:color ,vlink-underline :style line)))))
                            `(cursor ((t (:background ,cursor))))
                            `(show-paren-match ((t (:background ,paren-match-fg :foreground ,paren-match-bg))))
                            `(isearch ((t (:foreground ,search-fg :background ,search-bg))))
                            `(isearch-fail ((t (:background ,search-fail-bg))))
                            `(query-replace ((t (:inherit isearch))))
                            `(tooltip ((t (:inherit default :foreground ,tooltip-fg :background ,tooltip-bg))))
                            `(shadow ((t (:foreground ,shadow))))
                            `(secondary-selection ((t (:background ,secondary-bg))))
                            `(trailing-whitespace ((t (:background ,trailing-bg))))
                            `(lazy-highlight ((t (:foreground ,black :background ,white-light))))
                            `(next-error ((t (:inherit region))))
                            `(window-divider ((t (:background ,sep :foreground ,sep))))
                            `(vertical-border ((t (:background ,sep :foreground ,sep))))
                            `(minibuffer-prompt ((t (:inherit fixed-pitch :weight bold :foreground ,meta))))

                            ;; mode line
                            `(header-line         ((t (:overline nil :background ,header-line-bg :box (:line-width 1 :color "#8888cc") :underline ,sep :inherit mode-line))))

                            `(mode-line           ((t (:inherit fixed-pitch :foreground ,mode-line-fg       :background ,modeline :overline ,sep :box (:line-width 1 :color "#8888cc")))))
                            `(mode-line-inactive  ((t (:inherit fixed-pitch :foreground ,mode-line-inactive :background ,modeline :overline ,sep :box (:line-width 1 :color "#8888cc")))))

                            `(mode-line-buffer-id ((t (:weight bold))))
                            `(mode-line-emphasis  ((t (:weight bold))))
                            `(mode-line-highlight ((t (:background ,mode-line-hlt))))

                            `(error ((t (:inherit fixed-pitch))))

                            ;; primary font lock
                            `(font-lock-comment-face ((t (:foreground ,shadow :inherit fixed-pitch))))
                            `(font-lock-builtin-face ((t (:foreground ,builtin :inherit fixed-pitch))))
                            `(font-lock-string-face  ((t (:inherit fixed-pitch :foreground ,string))))
                            `(font-lock-function-name-face ((t (:inherit fixed-pitch :foreground ,function-name))))
                            `(font-lock-keyword-face ((t (:inherit fixed-pitch :weight bold :foreground ,keyword))))
                            `(font-lock-comment-delimiter-face ((t (:inherit fixed-pitch :inherit font-lock-comment-face))))
                            `(font-lock-constant-face ((t (:inherit fixed-pitch))))
                            `(font-lock-doc-face ((t (:inherit fixed-pitch :inherit font-lock-string-face))))
                            `(font-lock-preprocessor-face ((t (:inherit fixed-pitch :inherit font-lock-builtin-face))))
                            `(font-lock-regexp-grouping-backslash ((t (:inherit fixed-pitch :inherit bold))))
                            `(font-lock-regexp-grouping-construct ((t (:inherit fixed-pitch :inherit bold))))
                            `(font-lock-type-face ((t (:inherit fixed-pitch))))
                            `(font-lock-variable-name-face ((t (:inherit fixed-pitch :foreground ,variable))))
                            `(font-lock-warning-face ((t (:inherit error))))

                            ;; Org
                            `(org-level-1 ((t (:inherit default :foreground ,header :height ,(poet--height 1.5)))))
                            `(org-level-2 ((t (:inherit default :foreground ,header :height ,(poet--height 1.4)))))
                            `(org-level-3 ((t (:inherit default :foreground ,header :height ,(poet--height 1.3)))))
                            `(org-level-4 ((t (:inherit default :foreground ,header :height ,(poet--height 1.23)))))
                            `(org-level-5 ((t (:inherit default :foreground ,header :height ,(poet--height 1.23)))))
                            `(org-level-6 ((t (:inherit default :foreground ,header :height ,(poet--height 1.23)))))
                            `(org-level-7 ((t (:inherit default :foreground ,header :height ,(poet--height 1.23)))))
                            `(org-level-8 ((t (:inherit default :foreground ,header :height ,(poet--height 1.23)))))
                            `(org-meta-line ((t (:inherit fixed-pitch :foreground ,org-meta))))
                            `(org-document-info-keyword ((t (:inherit fixed-pitch :foreground ,org-document-info))))
                            `(org-document-info ((t (:inherit default :foreground ,org-document-info))))
                            `(org-verbatim ((t (:inherit fixed-pitch))))
                            `(org-table ((t (:inherit fixed-pitch :background ,org-table))))
                            `(org-formula ((t (:inherit org-table :height ,(poet--height 1)))))
                            `(org-quote ((t (:inherit default :foreground ,org-quote-fg :background ,org-quote-bg))))
                            `(org-hide ((t (:inherit fixed-pitch :foreground ,bg))))
                            `(org-indent ((t (:inherit org-hide))))
                            `(org-date ((t (:inherit fixed-pitch :foreground ,org-date :underline nil))))
                            `(org-document-title ((t (:inherit default :foreground ,org-title :height ,(poet--height 1.8) :underline (:color ,org-title-underline)))))
                            `(org-checkbox ((t (:inherit fixed-pitch :weight bold :foreground ,org-checkbox))))
                            `(org-scheduled ((t (:foreground ,org-scheduled))))
                            `(org-scheduled-today ((t (:foreground ,org-scheduled-today))))
                            `(org-done ((t (:inherit fixed-pitch :foreground ,org-done))))
                            `(org-todo ((t (:inherit fixed-pitch :foreground ,org-todo))))
                            `(org-tag ((t (:inherit fixed-pitch :height ,(poet--height 1) :foreground ,org-tag))))
                            `(org-block-begin-line ((t (:inherit fixed-pitch :background ,org-block-line))))
                            `(org-block-end-line ((t (:inherit fixed-pitch :background ,org-block-line))))
                            `(org-block ((t (:background ,org-block-bg :inherit fixed-pitch))))
                            `(org-priority ((t (:inherit fixed-pitch :weight normal))))
                            `(org-agenda-structure ((t (:foreground ,org-agenda-structure-fg :background ,org-agenda-structure-bg :overline ,org-agenda-structure-bg :underline ,org-agenda-structure-bg))))
                            `(org-agenda-date-weekend ((t (:inherit org-agenda-structure))))
                            `(org-agenda-date-today ((t (:foreground ,org-agenda-today-fg :overline ,org-agenda-today-bg :background ,org-agenda-today-bg :underline ,org-agenda-today-bg))))
                            `(org-special-keyword ((t (:inherit fixed-pitch :foreground ,org-special-keyword))))
                            `(org-scheduled-previously ((t (:foreground ,org-sched-prev))))
                            `(org-agenda-done ((t (:foreground ,org-agenda-done))))
                            `(org-footnote ((t (:foreground ,link))))
                            `(hl-line ((t (:background ,hl-line))))

                            ;; line numbers
                            `(line-number ((t (:inherit fixed-pitch :foreground ,linum))))
                            `(line-number-current-line ((t (:inherit fixed-pitch :foreground ,linum-hlt))))

                            ;; markdown
                            `(markdown-header-face-1 ((t (:foreground ,header :inherit default :height ,(poet--height 1.5)))))
                            `(markdown-header-face-2 ((t (:foreground ,header :inherit default :height ,(poet--height 1.4)))))
                            `(markdown-header-face-3 ((t (:foreground ,header :inherit default :height ,(poet--height 1.3)))))
                            `(markdown-header-face-4 ((t (:foreground ,header :inherit default :height ,(poet--height 1.23)))))
                            `(markdown-header-face-5 ((t (:foreground ,header :inherit default :height ,(poet--height 1.23)))))
                            `(markdown-header-face-6 ((t (:foreground ,header :inherit default :height ,(poet--height 1.23)))))
                            `(markdown-header-face-7 ((t (:foreground ,header :inherit default :height ,(poet--height 1.23)))))
                            `(markdown-header-face-8 ((t (:foreground ,header :inherit default :height ,(poet--height 1.23)))))
                            `(markdown-markup-face ((t (:inherit fixed-pitch :foreground ,markdown-markup))))
                            `(markdown-inline-code-face ((t (:inherit fixed-pitch))))
                            `(markdown-metadata-key-face ((t (:inherit fixed-pitch :height ,(poet--height 1) :foreground ,markdown-metadata))))
                            `(markdown-metadata-value-face ((t (:inherit fixed-pitch :height ,(poet--height 1) :foreground ,fg))))
                            `(markdown-language-keyword-face ((t (:foreground ,markdown-language))))
                            `(markdown-list-face ((t (:inherit fixed-pitch :foreground ,markdown-list))))
                            `(markdown-code-face ((t (:inherit fixed-pitch :foreground ,fg :background ,markdown-code-bg))))
                            `(markdown-pre-face ((t (:inherit fixed-pitch :color ,fg :background ,markdown-pre-bg))))
                            `(markdown-header-delimiter-face ((t (:inherit fixed-pitch :foreground ,markdown-header-delimiter))))
                            `(markdown-header-rule-face ((t (:inherit fixed-pitch :foreground ,markdown-header-delimiter))))
                            `(markdown-url-face ((t (:inherit fixed-pitch :foreground ,link))))

                            ;; ivy
                            `(ivy-current-match ((t (:inherit fixed-pitch :background ,hlt))))
                            `(ivy-minibuffer-match-face-1 ((t (:inherit fixed-pitch :foreground  ,cyan-dark))))
                            `(ivy-minibuffer-match-face-2 ((t (:inherit fixed-pitch :foreground ,red-dark))))
                            `(ivy-minibuffer-match-face-3 ((t (:inherit fixed-pitch :foreground ,blue-dark))))
                            `(ivy-minibuffer-match-face-4 ((t (:inherit fixed-pitch :foreground ,brown-dark))))

                            ;; imenu
                            `(imenu-list-entry-face-0 ((t (:foreground ,imenu))))
                            `(imenu-list-entry-face-1 ((t (:foreground ,imenu))))
                            `(imenu-list-entry-face-2 ((t (:foreground ,imenu))))
                            `(imenu-list-entry-face-3 ((t (:foreground ,imenu))))
                            `(imenu-list-entry-face-4 ((t (:foreground ,imenu))))
                            `(imenu-list-entry-face-5 ((t (:foreground ,imenu))))

                            ;; tldr
                            `(tldr-title       ((t (:foreground ,red :weight bold :height 1.9))))
                            `(tldr-description ((t (:foreground ,green))))
                            `(tldr-code-block  ((t ())))
                            `(tldr-command-argument ((t ())))

                            `(company-tooltip-selection        ((t (:background "#ddffdd"))))
                            `(company-tooltip-selection        ((t (:background "#cee7cf"))))
                            `(company-tooltip-common           ((t (:inherit fixed-pitch :foreground nil :underline nil :weight bold))))
                            `(company-scrollbar-fg             ((t (:background "#388E3C"))))
                            `(company-preview-common           ((t (:underline nil))))
                            `(company-tooltip-common           ((t (:underline nil))))
                            `(company-tooltip-common-selection ((t (:underline nil))))

                            ;; evil
                            `(evil-ex-substitute-replacement ((t (:foreground ,white-light :background ,brown-dark :underline nil))))
                            `(evil-goggles-delete-face ((t (:inherit 'lazy-highlight))))
                            `(evil-goggles-paste-face  ((t (:inherit 'lazy-highlight))))
                            `(evil-goggles-yank-face   ((t (:inherit 'lazy-highlight))))

                            `(helm-source-header ((t (:height ,(poet--height 1))))))

    (custom-theme-set-variables 'poet
                                '(line-spacing 0.4)
                                `(fci-rule-color ,fci))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'poet)
;;; poet-theme.el ends here
