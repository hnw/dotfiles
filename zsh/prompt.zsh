autoload colors && colors
# cheers, @ehrenmurdick
# http://github.com/ehrenmurdick/config/blob/master/zsh/prompt.zsh

if (( $+commands[git] ))
then
  git="$commands[git]"
else
  git="/usr/bin/git"
fi

git_dirty() {
    declare st
    st=$($git status --porcelain 2>/dev/null) || return
    if [[ $st == "" ]]
    then
        echo "on %{$fg_bold[green]%}$(git_prompt_info)%{$reset_color%}"
    else
        echo "on %{$fg_bold[red]%}$(git_prompt_info)%{$reset_color%}"
    fi
}

git_prompt_info () {
    declare ref
    ref=$($git symbolic-ref HEAD 2>/dev/null) || return
    echo "${ref#refs/heads/}"
}

need_push () {
    declare unpushed
    unpushed=$($git cherry -v @{upstream} 2>/dev/null) || return
    if [[ $unpushed == "" ]]
    then
        echo " "
    else
        echo " with %{$fg_bold[magenta]%}unpushed%{$reset_color%} "
    fi
}

ruby_version() {
  if (( $+commands[rbenv] ))
  then
    echo "$(rbenv version | awk '{print $1}')"
  fi

  if (( $+commands[rvm-prompt] ))
  then
    echo "$(rvm-prompt | awk '{print $1}')"
  fi
}

rb_prompt() {
  if ! [[ -z "$(ruby_version)" ]]
  then
    echo "%{$fg_bold[yellow]%}$(ruby_version)%{$reset_color%} "
  else
    echo ""
  fi
}

directory_name() {
  echo "%{$fg_bold[cyan]%}%1/%\/%{$reset_color%}"
}

# SSH接続時にホスト名をプロンプトに表示
PROMPT_SSH=""
if [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] ; then
  PROMPT_SSH="@%{$fg_bold[yellow]%}${HOST%%.*}%{$reset_color%} "
fi

case "$TERM" in
  dumb | emacs)
    export PROMPT="%m:%~> "
    ;;
  *)
    export PROMPT=$'\n${PROMPT_SSH}in $(directory_name) $(git_dirty)$(need_push)\n› '
    ;;
esac

set_prompt () {
  export RPROMPT="%{$fg_bold[cyan]%}%{$reset_color%}"
}

precmd() {
  title "zsh" "%m" "%55<...<%~"
  set_prompt
}
