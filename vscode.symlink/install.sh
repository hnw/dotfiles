#!/usr/bin/env bash

script_dir="$(cd "$(dirname ${BASH_SOURCE:-$0})"; pwd)"
src="$script_dir/settings.json"
dst="$HOME/Library/Application Support/Code/User/settings.json"

backup_and_link_file() {
    local src=$1 dst=$2

    if [ -f "$dst" ]; then
	local currentSrc="$(readlink "$dst")"
	if [ "$currentSrc" == "$src" ]; then
	    # do nothing
	    exit
	fi
	# backup
	mv "$dst" "${dst}.bak"
	echo "moved $dst to ${dst}.bak"
    else
	mkdir -p "$(dirname "$dst")"
    fi
    ln -s "$src" "$dst"
}

backup_and_link_file "$src" "$dst"
