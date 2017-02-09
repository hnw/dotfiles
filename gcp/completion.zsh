# Enables shell command completion for gcloud.
GOOGLE_CLOUD_SDK_ZSH_COMPLETION="$HOME/Library/google-cloud-sdk/completion.zsh.inc"
if [ -f "$GOOGLE_CLOUD_SDK_ZSH_COMPLETION" ]; then
   source "$GOOGLE_CLOUD_SDK_ZSH_COMPLETION"
fi
