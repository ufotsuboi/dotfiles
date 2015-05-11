fpath=(
    $HOME/.zsh.d/plugins/cd-gitroot(N-/)
    $HOME/.zsh.d/plugins/zsh-completions/src(N-/)
    $fpath
)

export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"

# 色付けを有効に
autoload -U colors && colors

############
# git
############

# cd-gitroot
autoload -Uz cd-gitroot

# git alias
alias gaa='git add .'
alias gst='git status'
alias gcm='git commit'
alias gbr='git branch'
alias gpl='git push'
alias gps='git pull'
alias cdg='cd-gitroot'

############
# completion
############

# 自動補完を有効にする
autoload -Uz compinit
compinit -u

# 拡張 glob を有効にする
setopt extended_glob

# <Tab> でパス名の補完候補を表示したあと、続けて <Tab> を押すと候補からパス名を選択できるようになる
zstyle ':completion:*:default' menu select=1

# 大文字小文字区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# 単語の一部として扱われる文字のセットを指定する
# ここではデフォルトのセットから / を抜いたものとする
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# 補完時の一覧の色付け
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'


############
# directory
############

alias md='mkdir -p'
alias rd=rmdir

# lsの色付け
export LSCOLORS="Gxfxcxdxbxegedabagacad"
zstyle ':completion:*' list-colors ''

# List directory contents
alias lsa='ls -lah'
alias l='ls -lah'
alias ll='ls -lh'
alias la='ls -lAh'
alias lr='ls -lrth'

# 入力したコマンドが存在せず、かつディレクトリ名と一致するなら、ディレクトリに cd する
setopt auto_cd

# 上記により...で二つ上のディレクトリに移動
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

# "~hoge" が特定のパス名に展開されるようにする（ブックマークのようなもの）
# hash -d hoge=/long/path/to/hogehoge

# cd した先のディレクトリをディレクトリスタックに追加する
# `cd +<Tab>` でディレクトリの履歴が表示され、そこに移動できる
setopt auto_pushd

# pushd したとき、ディレクトリがすでにスタックに含まれていればスタックに追加しない
setopt pushd_ignore_dups

############
# history
############

# history保存場所
export HISTFILE=${HOME}/.zsh_history

# メモリに保存される履歴の件数
export HISTSIZE=10000

# 履歴ファイルに保存される履歴の件数
export SAVEHIST=1000000

# コマンドの実行時間を記録する
setopt extended_history

# 古いものから先に削除する
setopt hist_expire_dups_first

# 以前と同じコマンドは保存しない
setopt hist_ignore_all_dups

# 直前のコマンドと同じコマンドは保存しない
setopt hist_ignore_dups

# コマンドがスペースで始まる場合、コマンド履歴に追加しない
setopt hist_ignore_space

# historyコマンドは履歴に登録しない
setopt hist_no_store

# 余分な空白は詰めて記録
setopt hist_reduce_blanks

# 古いコマンドと同じものは無視
setopt hist_save_no_dups

# historyを直接実行しない
setopt hist_verify

# 履歴をインクリメンタルに追加
setopt inc_append_history

# historyの共有
setopt share_history

# 途中入力で履歴からマッチするもののみを取得
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

############
# key-bind
############

# Use emacs key bindings
bindkey -e

# [Esc-l] - run command: ls
bindkey -s '\el' 'ls\n'

# [Ctrl-r] - Search backward incrementally for a specified string. The string may begin with ^ to anchor the search to the beginning of the line.
bindkey '^r' history-incremental-search-backward

# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
  bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi

# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
  bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi

# [Space] - do history expansion
bindkey ' ' magic-space

############
# prompt
############

setopt prompt_subst

# get the name of the branch we are on
function git_prompt_info() {
  if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" != "1" ]]; then
    ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
  fi
}

# Checks if working tree is dirty
parse_git_dirty() {
  local STATUS=''
  local FLAGS
  FLAGS=('--porcelain')
  if [[ "$(command git config --get oh-my-zsh.hide-dirty)" != "1" ]]; then
    if [[ $POST_1_7_2_GIT -gt 0 ]]; then
      FLAGS+='--ignore-submodules=dirty'
    fi
    if [[ "$DISABLE_UNTRACKED_FILES_DIRTY" == "true" ]]; then
      FLAGS+='--untracked-files=no'
    fi
    STATUS=$(command git status ${FLAGS} 2> /dev/null | tail -n1)
  fi
  if [[ -n $STATUS ]]; then
    echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  else
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  fi
}

local ret_status="%(?:%{$fg_bold[green]%}%n@%m:%{$fg_bold[red]%}%n@%m%s)"
PROMPT='${ret_status}%{$fg_bold[green]%}%p %{$fg[cyan]%}%c %{$fg_bold[blue]%}$(git_prompt_info)%{$fg_bold[blue]%} % %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX="git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"

############
# other
############

# anyenv
if [ -d ${HOME}/.anyenv ] ; then
    export PATH="$HOME/.anyenv/bin:$PATH"
    eval "$(anyenv init -)"
    # for D in `ls $HOME/.anyenv/envs`
    # do
    #     export PATH="$HOME/.anyenv/envs/$D/shims:$PATH"
    # done
fi


[ -f ~/.zshrc.mine ] && source ~/.zshrc.mine
