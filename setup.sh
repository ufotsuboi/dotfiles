#!/bin/bash
PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin

# スクリプトがあるディレクトリの絶対パス
dir=$(cd $(dirname $0);pwd)

# リンクを作成しないファイル名のリスト
ignorefiles=("." ".." ".zshrc.local" ".my.cnf" ".git" ".gitmodules" ".gitignore" "${0##*/}")

for dotfile in ${dir}/.?*; do
  is_ignored=0
  for ignorefile in ${ignorefiles[@]}; do
    if [ $dotfile = "${dir}/${ignorefile}" ]; then
      is_ignored=1
      break
    fi
  done

  if [ $is_ignored -ne 1 ]; then
    ln -s -v -f $dotfile $HOME
  fi
done
