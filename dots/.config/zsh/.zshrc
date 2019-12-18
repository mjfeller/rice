source $HOME/.config/oh-my-zsh/oh-my-zsh.sh
source $HOME/.config/zsh/fzf.zsh
source $HOME/.config/zsh/kubernetes.zsh
source $HOME/.config/zsh/recurly.zsh
source $HOME/.config/zsh/gcloud.zsh
source $HOME/.config/zsh/private.zsh
source $HOME/.config/aliases

# oh my zsh plugins
plugins=(
  colored-man-pages
  compleat
  git
)

# setup prompt
git_prompt() {
    ref=$(git_current_branch)
    if [ ! -z "$ref" ]; then
       echo "%F{cyan}$ref%f "
    fi
}
PROMPT='%F{241}λ %2~%f $(git_prompt)%B%F{241}»%b%f '

# history in cache directory
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history
SHELL_SESSION_HISTORY=0
