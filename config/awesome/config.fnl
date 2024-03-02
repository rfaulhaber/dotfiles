;; wip

(local gears (require "gears"))
(local awful (require "awful"))
(local wibox (require "wibox"))
(local beautiful (require "beautiful"))
(local naughty (require "naughty"))
(local ruled (require "ruled"))
(local hotkeys-popup (require "awful.hotkeys_popup"))

(require "awful.autofocus")
(require "awful.hotkeys_popup.keys")

(fn make-notification [urgency title message]
  {
   "urgency" urgency
   "title" (or title "Message")
   "message" message
  })

(naughty.connect_signal "request::display_error" (fn [message startup]
                                                   (naughty.notification
                                                    (make-notification "critical" "An error has occurred!" message))))
(beautiful.init (.. (gears.filesystem.get_themes_dir) "default/theme.lua"))
(set beautiful.font "Hack Nerd Font Mono 9")

(local terminal "kitty")
(local editor "emacs")
(local editor-cmd (.. terminal " -e " editor))
(local browser-cmd "firefox-developer-edition")
(local lock-exec "dm-tool lock")

(fn lock-screen []
  (awful.spawn lock-exec))

(local modkey "Mod4")
(local shift-key "Shift")
(local ctrl-key "Control")
(local alt-key "Mod1")

