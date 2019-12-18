(setenv "RECURLY" (substitute-in-file-name "$GOPATH/src/githum.com/recurly"))

(defvar redeam/backplane "https://backplane.develop.redeam.io/v1")

(defun recurly ()
  "open recurly go code direcrtory"
  (interactive)
  (counsel-find-file (getenv "RECURLY")))

(defun core ()
  "open file in core"
  (interactive)
  (counsel-find-file (getenv "CORE")))

(defun ops ()
  "open file in ops"
  (interactive)
  (counsel-find-file (substitute-in-file-name "$OPS/deploy/k8s/helm")))

(defvar redeam/env-list
  '("develop dev-test" "sandbox dev-test")
  "list of redeam environments used by the e2e runner")

(defvar redeam/clusters
  '("redeam-develop develop"
    "actourex-develop develop3"
    "redeam-sandbox sandbox"
    "redeam-staging -z us-west1-c staging"
    "actourex-production production"))

(defvar redeam/extra-service-alist
  '("channel-api-dragonheart")
  "services which do not conform to the -svc pattern in $CORE/cmd")

(defun redeam/set-cluster (CLUSTER)
  "Set kubeconfig to use CLUSTER"
  (interactive (list (ivy-read "Environment:" redeam/clusters)))
  (cd (substitute-in-file-name "$CORE/scripts/cluster"))
  (async-shell-command
   (format "./set-cluster.py -p %s" CLUSTER)))

(defun redeam/get-cluster ()
  "get the name of the cluster $HOME/cluster.config is using"
  (interactive)
  (shell-command "kubectl --kubeconfig $HOME/cluster.config config current-context"))

(defun trim-svc (s)
  "Remove trailing -svc commonly seen on redeam services"
  (car (split-string s "-svc" t "-svc")))

(defun redeam/services ()
  (cons redeam/extra-service-alist
        (directory-files (substitute-in-file-name "$CORE/cmd") nil "svc")))

(defun read-service (prompt)
  (ivy-read prompt (redeam/services)))

(defun redeam/e2e-booking (ENVIRONMENT)
  "run booking API end to end tests against the specified environment"
  (interactive (list (ivy-read "Environment:" redeam/env-list)))
  (cd (substitute-in-file-name "$CORE/api/surfaces/booking-api/workflows"))
  (async-shell-command
   (format "./build-and-run.sh %s" ENVIRONMENT) "*E2E Tests*"))

(defun redeam/build-image (SERVICE TAG)
  (interactive (list (read-service "Service:" )
                     (read-string "Image Tag: ")))
  (cd (substitute-in-file-name "$CORE"))
  (async-shell-command
   (format "./scripts/build.py --tag %s --service cmd/%s" TAG SERVICE)))

(defun redeam/inject (SERVICE)
  "inject a new freshly build service into a cluster"
  (interactive (list (trim-svc (read-service "Service: "))))
  (async-shell-command (format "inject %s" SERVICE)
                           (format "*Injecting %s*" SERVICE)))

(defun redeam/inject-many ()
  "inject many services into a cluster"
  (interactive)
  (while (redeam/inject (trim-svc (read-service "Service: ")))))

(defun mjf/async-shell-command (COMMAND &optional OUTPUT-BUFFER)
  (async-shell-command COMMAND OUTPUT-BUFFER)
  (switch-to-buffer OUTPUT-BUFFER)
  (local-set-key (kbd "q") 'kill-buffer-and-window)
  (evil-normal-state)
  (previous-buffer))

(defun mjf/pretty-json ()
  (interactive)
  (switch-to-buffer "*Pretty JSON*")
  (evil-insert-state)
  (local-set-key (kbd "q") 'kill-buffer-and-window)
  (delete-region (point-min) (point-max))
  (yank)
  (json-reformat-region (point-min) (point-max)))

(defun redeam/deft ()
  "open deft relative to CODE Documentation"
  (interactive)
  (let ((deft-directory (substitute-in-file-name "$CORE/Documentation"))
        (deft-recursive t))
    (deft)))

(provide 'redeam)
