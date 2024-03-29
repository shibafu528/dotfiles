# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/shibafu/.zshrc'

# alias
alias ls='ls --color=auto'
alias la='ls -a'
alias ll='la -l'
alias xc='xclip -i -selection clipboard'
alias ga='git add'
alias gb='git branch'
alias gs='git status'
alias gl='git log --decorate --graph'
alias glo='git log --decorate --graph --oneline'
alias gd='git diff'
alias gc='git commit'
alias gch='git checkout'
alias winerpg='WINEPREFIX=~/.wine/rpg_rt wine'
alias steam-wine='wine ~/.wine/drive_c/Program\ Files\ \(x86\)/Steam/Steam.exe -no-dwrite >/dev/null 2>&1 &'
alias be='bundle exec'
alias ber='bundle exec ruby'
alias dcon='docker-compose'
alias dcond='docker-compose down'
alias dconu='docker-compose up -d'
alias dconl='docker-compose logs -f'
alias unzip='unzip -Ocp932'

autoload -Uz compinit promptinit
autoload -U colors && colors
compinit
promptinit
PROMPT="[%n%B%{$fg[green]%}@%m%{$reset_color%}%b %~]$ "

setopt auto_pushd
setopt auto_list
setopt auto_menu
setopt auto_param_keys
setopt auto_param_slash
setopt hist_ignore_dups
setopt magic_equal_subst
setopt mark_dirs
setopt print_eight_bit
setopt share_history
setopt prompt_subst
setopt notify

# Key Bind Fix
bindkey -e
bindkey "^?"    backward-delete-char
bindkey "^H"    backward-delete-char
bindkey "^[[3~" delete-char
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line

# IM config
export GTK_IM_MODULE=fcitx5
export XMODIFIERS=@im=fcitx5
export QT_IM_MODULE=fcitx5
