GO_GAE_SDK_PATH="$HOME/Library/go_appengine"
if [ -d "$GO_GAE_SDK_PATH" ]; then
    PATH="$PATH:$GO_GAE_SDK_PATH"
fi
# Updates PATH for the Google Cloud SDK.
GOOGLE_CLOUD_SDK_ZSH_INC="$HOME/Library/google-cloud-sdk/path.zsh.inc"
if [ -f "$GOOGLE_CLOUD_SDK_ZSH_INC" ]; then
   source "$GOOGLE_CLOUD_SDK_ZSH_INC"
fi
