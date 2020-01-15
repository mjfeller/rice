alias k="kubectl"
alias kc="kubectl"
alias kd="kubectl describe"
alias kpw="kubectl get pods --watch"

pod() {
    kubectl get pods -l app.kubernetes.io/name=$1 -o json | jq -r '.items[0].metadata.name'
}

kcc() {
    ctx=`kubectl config get-contexts -o name | fzf`
    kubectl config use-context $ctx
}

authn() {
    istioctl authn -n qa5 tls-check $(pod recurly-app-web) | rg "HOST|qa5" --color=never
}

# kt() {
#     ktail -t '$BOLD$LIGHT_CYAN{{ .Container.Name }}$NONE $DIM{{ .Pod.Name }}$NONE $YELLOW{{ if .Timestamp }}{{ .Timestamp.Format \"2006-01-02T15:04:05.999999999Z07:00\" }}{{ end }}$NONE {{ .Message }}'
# }
