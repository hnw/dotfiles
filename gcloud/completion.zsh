# Enables shell command completion for gcloud.
completion_file='/opt/homebrew-cask/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'
if [ -f "$completion_file" ]; then
   source $path
fi
