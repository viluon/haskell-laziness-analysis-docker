#
# ~/.bashrc
#

export PATH="/home/packager/.cabal/bin:/home/packager/.ghcup/bin:$PATH"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls="exa -F --colour=auto --colour-scale --git --icons"
alias lh="ls -l"
PS1='[\u@\h \W]\$ '

