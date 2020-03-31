;;; helm.el --- extension for controlling Kubernetes with limited permissions -*- lexical-binding: t; -*-

;; Copyright (C) 2019, Mark Feller

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 0.1
;; Author: Mark Feller
;; Keywords: helm k8s tools processes
;; License: GNU General Public License >= 3
;; Package-Requires: ((transient "0.1.0") (emacs "25.3"))

;;; Commentary:

;;; Usage:

;; M-x helm

;;; Code:

(require 'transient)

(defvar helm-command "helm"
  "Helm binary")

(defvar kubectl-command "kubectl"
  "Kubectl binary")

(defconst helm--list-format
  [("Name"        30 t)
   ("Revision"    10 t)
   ("Updated"     30 t)
   ("Status"      10 t)
   ("Chart"       20 t)
   ("AppVersion"  10 t)
   ("Namespace"   10 t)]
  "List format.")

(defvar helm-context
  (replace-regexp-in-string
   "\n" "" (shell-command-to-string "kubectl config current-context"))
  "current context.  tries to smart default.")

(defun helm-set-context ()
  "Set the context."
  (interactive)
  (setq helm-context
        (completing-read
         "Select context: "
         (split-string (shell-command-to-string "kubectl config view -o jsonpath='{.contexts[*].name}'") " ")))
  (helm))

(defconst helm--list-sort-key
  '("Name" . nil)
  "Sort table on this key.")

(defun helm--get-context ()
  "Utility function to return the proper context and namespace arguments."
  (unless (equal helm-context "")
    (list "--kube-context" helm-context)))

(defun helm--buffer-name ()
  "Return helm buffer name."
  (concat "*helm [" helm-context "]*"))

(defun helm--propertize-status (status)
  "Return the status in proper font color.

STATUS is the pod status string."
  (let ((pair (cdr (assoc status helm--status-colors))))
    (if pair
        (propertize status 'font-lock-face `(:foreground ,pair))
      status)))

(defun helm--get-command-prefix ()
  "Utility function to prefix the kubectl command with proper context and namespace."
  (mapconcat 'identity (append '("helm") (helm--get-context)) " "))

(defun helm--list-entries ()
  "Create the entries for the service list."
  (let ((output (helm--exec-string '("ls --output json"))))
    (mapcar (lambda (x)
              (list (cdr (car x))
                    (vconcat (mapcar (lambda (y)
                                       (format "%s" (cdr y))) x))))
            (cdr (car (cdr (json-read-from-string output)))))))

(defun helm--exec (buffer-name async args)
  "Utility function to run commands in the proper context and namespace.

\"BUFFER-NAME\" is the buffer-name. Default to *helm-command*.
ASYNC is a bool. If true will run async.
ARGS is a ist of arguments."
  (when (equal buffer-name "")
    (setq buffer-name "*helm-command*"))
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name))
  (if async
      (apply #'start-process buffer-name buffer-name "helm" (append (helm--get-context) args))
    (apply #'call-process "helm" nil buffer-name nil (append (helm--get-context) args)))
  (pop-to-buffer buffer-name))

(defun helm--exec-string (args)
  ""
  (shell-command-to-string
   (mapconcat 'identity (append '("helm") (helm--get-context) args) " ")))

(defun helm--pop-to-buffer (name)
  "Utility function to pop to buffer or create it.

NAME is the buffer name."
  (unless (get-buffer name)
    (get-buffer-create name))
  (pop-to-buffer name))

(defun helm--get-chart-under-cursor ()
  "Utility function to get the name of the pod under the cursor."
  (aref (tabulated-list-get-entry) 0))

(defun helm-get-values (&optional args)
  "test"
  (interactive)
  (let* ((chart (helm--get-chart-under-cursor))
         (buffer-name (format "*Helm Values - %s*" chart)))
    (helm--exec buffer-name nil `("get" "values" ,chart))))

(defun helm-get-notes (&optional args)
  "test"
  (interactive)
  (let* ((chart (helm--get-chart-under-cursor))
         (buffer-name (format "*Helm Notes - %s*" chart)))
    (helm--exec buffer-name nil `("get" "notes" ,chart))))

(defun helm-get-manifest (&optional args)
  "test"
  (interactive)
  (let* ((chart (helm--get-chart-under-cursor))
         (buffer-name (format "*Helm Manifest - %s*" chart)))
    (helm--exec buffer-name nil `("get" "manifest" ,chart))))

(defun helm-get-hooks (&optional args)
  "test"
  (interactive)
  (let* ((chart (helm--get-chart-under-cursor))
         (buffer-name (format "*Helm Hooks - %s*" chart)))
    (helm--exec buffer-name nil `("get" "hooks" ,chart))))

(defun helm-get-status (&optional args)
  "test"
  (interactive)
  (let* ((chart (helm--get-chart-under-cursor))
         (buffer-name (format "*Helm Status - %s*" chart)))
    (helm--exec buffer-name nil `("status" ,chart))))

(defun helm-get-history (&optional args)
  "test"
  (interactive)
  (let* ((chart (helm--get-chart-under-cursor))
         (buffer-name (format "*Helm History - %s*" chart)))
    (helm--exec buffer-name nil `("history" ,chart))))

(defun helm-rollback (revision &optional args)
  "test"
  (interactive (list (read-string "Revision: ")
                     (transient-args 'helm-rollback-popup)))
  (let* ((chart (helm--get-chart-under-cursor))
         (buffer-name (format "*Helm History - %s*" chart)))
    (message "Revision:" revision)
    (let ((arg (append '("rollback") args (list chart revision))))
      (helm--exec buffer-name nil arg))))

(defun helm-delete (&optional args)
  "test"
  (interactive)
  (let* ((chart (helm--get-chart-under-cursor))
         (buffer-name (format "*Helm History - %s*" chart)))
    (helm--exec buffer-name nil `("delete" ,chart))))

(defun helm-search (&optional args)
  "test"
  (interactive)
  (let* ((chart (helm--get-chart-under-cursor))
         (buffer-name (format "*Helm History - %s*" chart)))
    (helm--exec buffer-name nil (list "search"))))

(defun helm-repo-list (&optional args)
  "test"
  (interactive)
  (let* ((chart (helm--get-chart-under-cursor))
         (buffer-name (format "*Helm Repo Add - %s*" chart)))
    (helm--exec buffer-name nil (list "repo" "list"))))

(defun helm-repo-list (&optional args)
  "test"
  (interactive)
  (let* ((chart (helm--get-chart-under-cursor))
         (buffer-name (format "*Helm Repo Add - %s*" chart)))
    (helm--exec buffer-name nil (list "repo" "update"))))

(defun helm-version (&optional args)
  (interactive (list (transient-args 'helm-version-popup)))
  (let ((buffer-name (format "*Helm Version*")))
    (if (member "--short" args)
        (message (helm--exec-string (append '("version") args)))
      (helm--exec buffer-name nil (append '("version") args)))))

;; ----------------------------------------------------------------------
;;  popups
;; ----------------------------------------------------------------------

(define-transient-command helm-history-popup ()
  "Helm History Menu"
  ["Arguments"
   ("-y" "YAML" "-o yaml")
   ("-j" "JSON" "-o json")
   ("--max" "Max revisions" "--max=10")]
  ["Actions"
   ("h" "History" helm-get-history)])

(define-transient-command helm-get-popup ()
  "Helm Values Menu"
  ["Arguments"
   ("-r" "revision" "--revision=")]
  ["Actions"
   ("v" "Values" helm-get-values)
   ("n" "Notes" helm-get-notes)
   ("m" "Manifest" helm-get-manifest)
   ("h" "Hooks" helm-get-hooks)])

(define-transient-command helm-repo-popup ()
  "Helm Values Menu"
  ["Arguments"
   ("-r" "revision" "--revision=")]
  ["Actions"
   ;; ("a" "Add" helm-repo-add)
   ;; ("i" "Index" helm-repo-index)
   ("l" "List" helm-repo-list)
   ;; ("r" "Remove" helm-repo-remove)
   ("u" "Update" helm-repo-update)
   ])

(define-transient-command helm-rollback-popup ()
  "Helm Rollback Menu"
  ["Arguments"
   ("-d" "simulate a rollback" "--dry-run")
   ("-f" "force resource update" "--force")
   ("-r" "performs pods restart if applicable" "--recreate-pods")
   ("-D" "specify a description for he release" "--description=")]
  ["Actions"
   ("r" "Rollback" helm-rollback)])

(define-transient-command helm-delete-popup ()
  "Helm Rollback Menu"
  ["Arguments"
   ("-d" "simulate a rollback" "--dry-run")
   ("-p" "remove the release from the store and make its name free for later use" "--purge")
   ("-D" "specify a description for he release" "--description=")]
  ["Actions"
   ("k" "Delete" helm-delete)])

(define-transient-command helm-values-popup ()
  "Helm Values Menu"
  ["Arguments"
   ("-y" "YAML" "-o yaml")
   ("-j" "JSON" "-o json")
   ("-a" "dump all (computed) values" "--all")
   ("-r" "revision" "--revision=")]
  ["Actions"
   ("v" "Values" helm-get-values)])

(define-transient-command helm-status-popup ()
  "Helm Values Menu"
  ["Arguments"
   ("-y" "YAML" "-o yaml")
   ("-j" "JSON" "-o json")
   ("-r" "Revision" "--revision=")]
  ["Actions"
   ("s" "Status" helm-get-status)])

(define-transient-command helm-search-popup ()
  "Helm Search Packges Menu"
  ["Arguments"
   ("-y" "yaml output format" "-o yaml")
   ("-j" "json output format" "-o json")
   ("-l" "long listings with each version on it's own line" "--versions")
   ("-r" "Regexp" "")]
  ["Actions"
   ("S" "Search" helm-search)])

(define-transient-command helm-version-popup ()
  "Helm Version Menu"
  ["Arguments"
   ("-c" "client version only" "--client")
   ("-s" "server version only" "--server")
   ("-S" "print the version number" "--short")]
  ["Actions"
   ("V" "Version" helm-version)])

(define-transient-command helm-help-popup ()
  "Helm Menu"
  ["Transient Commands"
   [("s" "Status"   helm-status-popup)
    ("v" "Values"   helm-values-popup)
    ("d" "Describe" helm-get-popup)]
   [("h" "History"  helm-history-popup)
    ("R" "Rollback" helm-rollback-popup)
    ("k" "Delete"   helm-delete-popup)]
   [("r" "Repo"     helm-repo-popup)
    ("S" "Search"   helm-search-popup)]]
  ["Config"
   ("C" "set kubernetes context" helm-set-context)
   ("V" "get helm version information" helm-version-popup)]
  ["Essential commands"
   ("g" "refresh current buffer"  helm)
   ("?" "helm menu" helm-help-popup)])

;; mode map
(setq helm-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C") 'helm-set-context)
        (define-key map (kbd "d") 'helm-get-popup)
        (define-key map (kbd "v") 'helm-values-popup)
        (define-key map (kbd "r") 'helm-repo-popup)
        (define-key map (kbd "R") 'helm-rollback-popup)
        (define-key map (kbd "k") 'helm-delete-popup)
        (define-key map (kbd "s") 'helm-status-popup)
        (define-key map (kbd "S") 'helm-search-popup)
        (define-key map (kbd "V") 'helm-version-popup)
        (define-key map (kbd "h") 'helm-history-popup)
        (define-key map (kbd "?") 'helm-help-popup)
        map))

;;;###autoload
(defun helm ()
  "Invoke the helm buffer."
  (interactive)
  (helm--pop-to-buffer (helm--buffer-name))
  (helm-mode))

(define-derived-mode helm-mode tabulated-list-mode "Helm"
  "Special mode for helm buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Helm")
  (setq major-mode 'helm-mode)
  (use-local-map helm-mode-map)
  (setq tabulated-list-format helm--list-format)
  (setq tabulated-list-entries 'helm--list-entries)
  (setq tabulated-list-sort-key helm--list-sort-key)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1)
  (run-mode-hooks 'helm-mode-hook))

(provide 'helm)
;;; helm.el ends here
