alias k="kubectl"
alias kd="kubectl describe"
alias kpw="kubectl get pods --watch"

[ -f "$HOME.kube/completion.zsh.inc" ] && source "$HOME.kube/completion.zsh.inc" 

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
