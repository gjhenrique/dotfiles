KUBE_PS1_SYMBOL_ENABLE=false
# kubeon to enable it if disabled
KUBE_PS1_ENABLED=on

GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="auto"
GIT_PS1_SHOWCOLORHINTS=1

# prefix and suffix Controlled by theme
ZSH_THEME_AWS_PREFIX=""
ZSH_THEME_AWS_SUFFIX=""

local user_host='%{$terminfo[bold]$fg_bold[yellow]%}%n%{$reset_color%} at %{$terminfo[bold]$fg_bold[red]%}%m%{$reset_color%}'
local current_dir='%{$terminfo[bold]$fg_bold[magenta]%} %~%{$reset_color%}'
local ret_status='%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}%? ➜ %s)%{$reset_color%}'
local git_branch='%{$fg_bold[cyan]%}$(__git_ps1)%{$reset_color%}'
local kube='%{$terminfo[bold]%}$(kube_ps1)%{$reset_color%}'
local aws='%{$terminfo[bold]%}$(aws_prompt_info)%{$reset_color%}'

PROMPT="${user_host} in${current_dir} ${git_branch} ${kube} ${aws}
${ret_status}"

