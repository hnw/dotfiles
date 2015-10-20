# Updates PATH for the Google Cloud SDK.
path_file='/opt/homebrew-cask/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
if [ -f "$path_file" ]; then
   source $path
fi
