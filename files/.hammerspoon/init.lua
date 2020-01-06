spaces = require("hs._asm.undocumented.spaces")
myLog = hs.logger.new('mymodule','debug')

hs.logger.defaultLogLevel = 'info'

function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ',\n'
      end
      return s .. '}\n '
   else
      return tostring(o)
   end
end

hs.hotkey.bind({"ctrl", "alt"}, "r", function()
      hs.reload()
end)

hs.hotkey.bind({"alt"}, "p", function()
      hs.spotify.playpause()
end)

hs.hotkey.bind({"ctrl", "alt"}, "u", function()
      hs.spotify.previous()
end)

hs.hotkey.bind({"ctrl", "alt"}, "i", function()
      hs.spotify.next()
end)

hs.hotkey.bind({"alt"}, "m", function()
      hs.caffeinate.systemSleep()
end)

-- from https://gist.github.com/spinscale/fd82f00da29447990f27f36b3f4b927d
function changeVolume(diff)
   return function()
      local current = hs.audiodevice.defaultOutputDevice():volume()
      local new = math.min(100, math.max(0, math.floor(current + diff)))
      if new > 0 then
         hs.audiodevice.defaultOutputDevice():setMuted(false)
      end
      hs.alert.closeAll(0.0)
      hs.alert.show("Volume " .. new .. "%", {}, 0.5)
      hs.audiodevice.defaultOutputDevice():setVolume(new)
   end
end

hs.hotkey.bind({'alt', 'ctrl'}, 'a', changeVolume(-3))
hs.hotkey.bind({'ctrl', 'alt'}, 's', changeVolume(3))

function registerWs(appName, title)
   local filterOption
   if title == null then
      filterOption = {visible=null}
   else
      filterOption = {allowTitles=title, visible=null}
   end

   local filter = hs.window.filter.new(appName):setAppFilter(appName, filterOption)

   return function()
      local win = filter:getWindows()[1]
      if win == null then
         myLog.w("window not found")
         return
      end

      local space = win:spaces()[1]
      myLog.i(dump(win:spaces()))
      if win:title() ~= hs.window.focusedWindow()then
         myLog.i("Focusing window " .. win:title())
         win:focus()
      end
   end
end

-- hs.window.filter.forceRefreshOnSpaceChange = true

-- function makeFullScreen(applicationName, title)
--    local filterOption
--    if title == null then
--       filterOption = {visible=null}
--    else
--       filterOption = {allowTitles=title, visible=null}
--    end

--    local filter = hs.window.filter.new(applicationName):setAppFilter(applicationName, filterOption)
--    local win = filter:getWindows()[1]
--    myLog.i("vixi" .. win:title())
--    win:setFullScreen(true)
-- end

-- makeFullScreen("Emacs", "EmacsPrimary")

-- toggle full screen also
hs.hotkey.bind('alt', '1', registerWs("Emacs", "EmacsPrimary"))
hs.hotkey.bind('alt', '2', registerWs("Emacs", "EmacsSecondary"))
hs.hotkey.bind('alt', '3', registerWs("Firefox Developer Edition"))
hs.hotkey.bind('alt', '4', registerWs("iTerm2"))
hs.hotkey.bind('alt', '5', registerWs("Google Chrome"))
hs.hotkey.bind('alt', 'q', registerWs("Books"))
hs.hotkey.bind('alt', '6', registerWs("Emacs", "EmacsNotes"))
hs.hotkey.bind('alt', '7', registerWs("Emacs", "EmacsGit"))
hs.hotkey.bind('alt', '8', registerWs("Emacs", "EmacsCompilation"))
hs.hotkey.bind('alt', '9', registerWs("Slack"))
hs.hotkey.bind('alt', '0', registerWs("Spotify", "Spotify Premium"))

hs.hotkey.bind({"ctrl", "alt"}, "z", function()
      local filter = hs.window.filter.new{"Emacs", "iTerm2", "Spotify", "Slack", "Firefox Developer Edition"}
      local wins = filter:getWindows()
      myLog.i(dump(wins))
      for i, win in ipairs(wins) do
         win:setFullScreen(true)
      end
end)

hs.window.filter.new{'Emacs'}:setAppFilter('Emacs', {allowTitles='EmacsPrimary'}):subscribe(
   hs.window.filter.windowFocused,
   function()
      myLog.i("Focusing")
   end
)
hs.window.filter.new{'Emacs'}:setAppFilter('Emacs', {allowTitles='EmacsSecondary'}):subscribe(
   hs.window.filter.windowFocused,
   function()
      myLog.i("Focusing")
   end
)
hs.window.filter.new{'Emacs'}:setAppFilter('Emacs', {allowTitles='EmacsGit'}):subscribe(
   hs.window.filter.windowFocused,
   function()
      myLog.i("Focusing")
   end
)

hs.window.filter.new{'Emacs'}:setAppFilter('Emacs', {allowTitles='EmacsNotes'}):subscribe(
   hs.window.filter.windowFocused,
   function()
      myLog.i("Focusing")
   end
)

hs.window.filter.new{'Emacs'}:setAppFilter('Emacs', {allowTitles='EmacsCompilation'}):subscribe(
   hs.window.filter.windowFocused,
   function()
      myLog.i("Focusing")
   end
)

function showWindows(appName, title)
   local filter2 = hs.window.filter.new(appName):setAppFilter(appName, {allowTitles=title, visible=null})
   -- filter2:setAppFilter(appName, {allowTitles=title})
   -- filter2:setAppFilter(appName)
   return filter2:getWindows()
end
