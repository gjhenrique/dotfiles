# ZSH Theme - Preview: http://gyazo.com/8becc8a7ed5ab54a0262a470555c3eed.png

local user_host='%{$terminfo[bold]$fg_bold[blue]%}%n@%m%{$reset_color%}'
local current_dir='%{$terminfo[bold]$fg_bold[cyan]%} %~%{$reset_color%}'
local git_branch='$(git_prompt_info)%{$reset_color%}'
local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}%? ➜ %s)%{$reset_color%}"

PROMPT="${user_host} ${current_dir} ${git_branch} ${ret_status}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[magenta]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg_bold[magenta]%})%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%} ✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%} ✔%{$reset_color%}"
