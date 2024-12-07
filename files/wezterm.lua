local config = wezterm.config_builder()

config.color_scheme = 'Dracula'
config.font = wezterm.font('JetBrains Mono')
config.font_size = 14.0

-- don't conflict with tmux yet
-- config.keys = {
--   {
--     key = 'c',
--     mods = 'META',
--     action = wezterm.action.SpawnTab 'CurrentPaneDomain',
--   },
--   {
--     key = 'k',
--     mods = 'META',
--     action = wezterm.action.ActivateTabRelative(1),
--   },
--   {
--     key = 'j',
--     mods = 'META',
--     action = wezterm.action.ActivateTabRelative(-1),
--   },
-- }

return config
