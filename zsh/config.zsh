if [[ -n $SSH_CONNECTION ]]; then
  export PS1='%m:%3~$(git_info_for_prompt)%# '
else
  export PS1='%3~$(git_info_for_prompt)%# '
fi

export LSCOLORS="exfxcxdxbxegedabagacad"
export CLICOLOR=true

fpath=($ZSH/functions $fpath)

autoload -U $ZSH/functions/*(:t)

HISTFILE=~/.zsh_history
# メモリ内の履歴の数
HISTSIZE=100000
# 保存される履歴の数
SAVEHIST=100000

setopt NO_BG_NICE # don't nice background tasks
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS # allow functions to have local options
setopt LOCAL_TRAPS # allow functions to have local traps
setopt HIST_VERIFY
setopt SHARE_HISTORY # share history between sessions ???
# 履歴ファイルに時刻を記録
setopt EXTENDED_HISTORY
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD
setopt IGNORE_EOF

setopt APPEND_HISTORY # adds history
# 履歴を他のシェルとリアルタイム共有・ヒストリを逐次追記
setopt INC_APPEND_HISTORY SHARE_HISTORY
# 直前と同じコマンドラインはヒストリに追加しない
setopt HIST_IGNORE_ALL_DUPS
# 余計なスペースを排除してヒストリに追加
setopt HIST_REDUCE_BLANKS

# history (fc -l) コマンドをヒストリリストから取り除く
setopt HIST_NO_STORE

# don't expand aliases _before_ completion has finished
#   like: git comm-[tab]
setopt complete_aliases

zle -N newtab

### キー操作
# おおむねEmacsっぽく
bindkey -e
# M-f/M-bの挙動をEmacsのそれに合わせる
bindkey "^[f" emacs-forward-word
bindkey "^[b" emacs-backward-word
# M-f/M-b/M-dなどでの単語境界の基準をEmacsライクに
export WORDCHARS=""
# M-n/M-pの挙動をtcshっぽく
bindkey "^[n" history-beginning-search-forward
bindkey "^[p" history-beginning-search-backward

bindkey '^[^[[D' backward-word
bindkey '^[^[[C' forward-word
bindkey '^[[5D' beginning-of-line
bindkey '^[[5C' end-of-line
bindkey '^[[3~' delete-char
bindkey '^[^N' newtab
bindkey '^?' backward-delete-char
