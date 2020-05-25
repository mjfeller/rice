export GCLOUD_SDK="$HOME/prog/google-cloud-sdk"

[ -f "$GCLOUD_SDK/path.zsh.inc" ]       && source "$GCLOUD_SDK/path.zsh.inc"
[ -f "$GCLOUD_SDK/completion.zsh.inc" ] && source "$GCLOUD_SDK/completion.zsh.inc"

gac() {
	cfg=$(gcloud config configurations list | awk 'NR!=-1 { print $1 }' | fzf -e)
	[ -z "$cfg" ] || gcloud config configurations activate $cfg
}

gssh() {
	instance=$(gcloud compute instances list | awk '/^gke/ { print $1 }' | sort | fzf)
	[ -z "$instance" ] || gcloud compute ssh $instance
}

gsp() {
	project=$(gcloud projects list | awk 'NR!=1 { print $1 }' | sort | fzf)
	[ -z "$project" ] || gcloud config set project $project
}
