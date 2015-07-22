ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

alias ohmyzsh="vim ~/.oh-my-zsh"
alias zshconfig="vim ~/.zshrc"
alias zshsource="source ~/.zshrc"
alias vimconfig="vim ~/.vimrc"
alias vimsource="source ~/.vimrc"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion # COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"
#

# Uncomment following line if you want to  shown in the command execution time stamp 
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
# HIST_STAMPS="mm/dd/yyyy"

export LANGUAGE=en_US.UTF8
export LANG=en_US.UTF8
export LC_ALL=en_US.UTF8

# Disable <C-q> e <C-s>
stty stop ''
stty start ''
stty -ixon
stty -ixoff


# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git gem history-substring-search fasd vagrant mvn node npm docker)

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

function gi() { curl https://www.gitignore.io/api/$@ ;}

function git_prompt_info() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}${ZSH_THEME_GIT_PROMPT_CLEAN}${ZSH_THEME_GIT_PROMPT_SUFFIX}".
}

function mako_system_cm() {
    adb reboot-booloader
    sudo fastboot flash system /home/guilherme/Projects/CM/out/target/product/mako/system.img
    sudo fastboot reboot
}

# If tmux is enabled, then startx in invoked, it gives the error xf86OpenConsole: VT_ACTIVATE failed: Operation not permitted
# Therefore, it only launches tmux when there is a X window
if [ "$TERM" != "linux" ]
then
  # Start shell with tmux
  # If not running interactively, do not do anything
  [[ $- != *i* ]] && return
  [[ -z "$TMUX" ]] && exec tmux -2
fi

# User configuration
alias xc="xclip -sel clip"
alias xco="xclip -o -sel clip"
alias shortgit="less /home/guilherme/.oh-my-zsh/plugins/git/git-aliases"
alias pg="ping google.com"
alias youtube-dl="noglob youtube-dl"
alias sv="sudo gvim"
alias ranger="TERM=xterm-256color ranger"
alias source_uel="source /home/guilherme/.proxy_uel_cred"

source $ZSH/oh-my-zsh.sh

# Highlight de código no less
export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
export LESS=' -R'
export JAVA_HOME="/usr/lib/jvm/java-7-openjdk"
export MAVEN_HOME="/opt/maven/"
export ANT_HOME="/opt/ant"
export ANDROID_HOME="/opt/sdk-android" 
export SCALA_HOME="/opt/scala"
export SCRIPTS_HOME="/opt/scripts"
export GRADLE_HOME="/opt/gradle"
export VAGRANT_HOME="/opt/vagrant"
export ACTIVATOR_HOME="/opt/activator"
export SBT_HOME="/opt/sbt"
export AVR_HOME="/usr/share/arduino/hardware/tools/avr"
export NDK_HOME="/opt/android-ndk"

export PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games"
export PATH="$MAVEN_HOME/bin:$JAVA_HOME/bin:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:$SCALA_HOME/bin:$SCRIPTS_HOME:$GRADLE_HOME/bin:$VAGRANT_HOME/bin:$ANT_HOME/bin:$ACTIVATOR_HOME:$SBT_HOME/bin:$AVR_HOME/bin:$PATH"

# export MANPATH="/usr/local/man:$MANPATH"
# Preferred editor for local and remote sessions
export EDITOR='vim'

export NVM_DIR="/home/guilherme/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm

export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

alias grep="/usr/bin/grep $GREP_OPTIONS"
unset GREP_OPTIONS

alias nw='nmcli dev wifi'
