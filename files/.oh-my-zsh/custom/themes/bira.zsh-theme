# ZSH Theme - Preview: http://gyazo.com/8becc8a7ed5ab54a0262a470555c3eed.png

GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="auto"
GIT_PS1_SHOWCOLORHINTS=1

local user_host='%{$terminfo[bold]$fg_bold[yellow]%}%n%{$reset_color%} at %{$terminfo[bold]$fg_bold[red]%}%m%{$reset_color%}'
local current_dir='%{$terminfo[bold]$fg_bold[magenta]%} %~%{$reset_color%}'
local git_branch='%{$fg_bold[cyan]%}$(__git_ps1)%{$reset_color%}'
local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}%? ➜ %s)%{$reset_color%}"

PROMPT="${user_host} in${current_dir} ${git_branch}
${ret_status}"
