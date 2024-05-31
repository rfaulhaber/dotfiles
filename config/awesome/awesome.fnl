(pcall require :luarocks.loader)
(local gears (require :gears))
(local awful (require :awful))
(require :awful.autofocus)
(local wibox (require :wibox))
(local beautiful (require :beautiful))
(local naughty (require :naughty))
(local ruled (require :ruled))
(local menubar (require :menubar))
(local hotkeys-popup (require :awful.hotkeys_popup))
(require :awful.hotkeys_popup.keys)
(naughty.connect_signal "request::display_error"
                        (fn [message startup]
                          (naughty.notification {: message
                                                 :title (.. "Oops, an error happened"
                                                            (or (and startup
                                                                     " during startup!")
                                                                "!"))
                                                 :urgency :critical})))

(local constants (require :constants))
(local launcher (require :components.top_bar.launcher))
(beautiful.init (.. (gears.filesystem.get_themes_dir) :default/theme.lua))
(set beautiful.font constants.font)
(fn lock-screen [] (awful.spawn constants.lock_exec))
(global modkey constants.keys.modkey)
(set menubar.utils.terminal constants.terminal)
(tag.connect_signal "request::default_layouts"
                    (fn []
                      (awful.layout.append_default_layouts [awful.layout.suit.floating
                                                            awful.layout.suit.tile
                                                            awful.layout.suit.tile.left
                                                            awful.layout.suit.tile.bottom
                                                            awful.layout.suit.tile.top
                                                            awful.layout.suit.fair
                                                            awful.layout.suit.fair.horizontal
                                                            awful.layout.suit.spiral
                                                            awful.layout.suit.spiral.dwindle
                                                            awful.layout.suit.max
                                                            awful.layout.suit.max.fullscreen
                                                            awful.layout.suit.magnifier
                                                            awful.layout.suit.corner.nw])))

(global mykeyboardlayout (awful.widget.keyboardlayout))
(global mytextclock
        (wibox.widget {:format "%a, %b %d %I:%M:%S %p"
                       :refresh 1
                       :widget wibox.widget.textclock}))

(local month-calendar (awful.widget.calendar_popup.month))
(month-calendar:attach mytextclock :tr)
(screen.connect_signal "request::desktop_decoration"
                       (fn [s]
                         (awful.tag [:1 :2 :3 :4 :5 :6 :7 :8 :9] s
                                    (. awful.layout.layouts 1))
                         (set s.mypromptbox (awful.widget.prompt))
                         (set s.mylayoutbox
                              (awful.widget.layoutbox {:buttons [(awful.button {}
                                                                               1
                                                                               (fn []
                                                                                 (awful.layout.inc 1)))
                                                                 (awful.button {}
                                                                               3
                                                                               (fn []
                                                                                 (awful.layout.inc (- 1))))
                                                                 (awful.button {}
                                                                               4
                                                                               (fn []
                                                                                 (awful.layout.inc (- 1))))
                                                                 (awful.button {}
                                                                               5
                                                                               (fn []
                                                                                 (awful.layout.inc 1)))]
                                                       :screen s}))
                         (set s.mytaglist
                              (awful.widget.taglist {:buttons [(awful.button {}
                                                                             1
                                                                             (fn [t]
                                                                               (t:view_only)))
                                                               (awful.button [modkey]
                                                                             1
                                                                             (fn [t]
                                                                               (when client.focus
                                                                                 (client.focus:move_to_tag t))))
                                                               (awful.button {}
                                                                             3
                                                                             awful.tag.viewtoggle)
                                                               (awful.button [modkey]
                                                                             3
                                                                             (fn [t]
                                                                               (when client.focus
                                                                                 (client.focus:toggle_tag t))))
                                                               (awful.button {}
                                                                             4
                                                                             (fn [t]
                                                                               (awful.tag.viewprev t.screen)))
                                                               (awful.button {}
                                                                             5
                                                                             (fn [t]
                                                                               (awful.tag.viewnext t.screen)))]
                                                     :filter awful.widget.taglist.filter.all
                                                     :screen s}))
                         (set s.mytasklist
                              (awful.widget.tasklist {:buttons [(awful.button {}
                                                                              1
                                                                              (fn [c]
                                                                                (c:activate {:action :toggle_minimization
                                                                                             :context :tasklist})))
                                                                (awful.button {}
                                                                              3
                                                                              (fn []
                                                                                (awful.menu.client_list {:theme {:width 250}})))
                                                                (awful.button {}
                                                                              4
                                                                              (fn []
                                                                                (awful.client.focus.byidx (- 1))))
                                                                (awful.button {}
                                                                              5
                                                                              (fn []
                                                                                (awful.client.focus.byidx 1)))]
                                                      :filter awful.widget.tasklist.filter.currenttags
                                                      :screen s}))
                         (set s.mywibox
                              (awful.wibar {:position :top
                                            :screen s
                                            :widget {1 {1 launcher
                                                        2 s.mytaglist
                                                        3 s.mypromptbox
                                                        :layout wibox.layout.fixed.horizontal}
                                                     2 s.mytasklist
                                                     3 {1 mykeyboardlayout
                                                        2 (wibox.widget.systray)
                                                        3 mytextclock
                                                        4 s.mylayoutbox
                                                        :layout wibox.layout.fixed.horizontal}
                                                     :layout wibox.layout.align.horizontal}}))))

(awful.mouse.append_global_mousebindings [(awful.button {} 4 awful.tag.viewprev)
                                          (awful.button {} 5 awful.tag.viewnext)])

(awful.keyboard.append_global_keybindings [(awful.key [modkey] :s
                                                      hotkeys-popup.show_help
                                                      {:description "show help"
                                                       :group :awesome})
                                           (awful.key [modkey :Control] :r
                                                      awesome.restart
                                                      {:description "reload awesome"
                                                       :group :awesome})
                                           (awful.key [modkey :Shift] :q
                                                      awesome.quit
                                                      {:description "quit awesome"
                                                       :group :awesome})
                                           (awful.key [modkey] :x
                                                      (fn []
                                                        (awful.prompt.run {:exe_callback awful.util.eval
                                                                           :history_path (.. (awful.util.get_cache_dir)
                                                                                             :/history_eval)
                                                                           :prompt "Run Lua code: "
                                                                           :textbox (. (. (awful.screen.focused)
                                                                                          :mypromptbox)
                                                                                       :widget)}))
                                                      {:description "lua execute prompt"
                                                       :group :awesome})
                                           (awful.key [modkey] :Return
                                                      (fn []
                                                        (awful.spawn constants.terminal
                                                                     {:maximized true
                                                                      :tag :3}))
                                                      {:description "open a terminal"
                                                       :group :launcher})
                                           (awful.key [modkey] :r
                                                      (fn []
                                                        (: (. (awful.screen.focused)
                                                              :mypromptbox)
                                                           :run))
                                                      {:description "run prompt"
                                                       :group :launcher})
                                           (awful.key [modkey] :p
                                                      (fn [] (menubar.show))
                                                      {:description "show the menubar"
                                                       :group :launcher})
                                           (awful.key [modkey] :e
                                                      (fn []
                                                        (awful.spawn "emacsclient -c"
                                                                     {:maximized true
                                                                      :tag :2}))
                                                      {:description "start emacs"
                                                       :group :client})
                                           (awful.key [modkey] :b
                                                      (fn []
                                                        (awful.spawn constants.browser_cmd
                                                                     {:maximized true
                                                                      :tag :1}))
                                                      {:description "start firefox"
                                                       :group :client})
                                           (awful.key [modkey] :l lock-screen
                                                      {:description "lock screen"
                                                       :group :client})])

(awful.keyboard.append_global_keybindings [(awful.key [modkey] :Left
                                                      awful.tag.viewprev
                                                      {:description "view previous"
                                                       :group :tag})
                                           (awful.key [modkey] :Right
                                                      awful.tag.viewnext
                                                      {:description "view next"
                                                       :group :tag})
                                           (awful.key [modkey] :Escape
                                                      awful.tag.history.restore
                                                      {:description "go back"
                                                       :group :tag})])

(awful.keyboard.append_global_keybindings [(awful.key [modkey] :j
                                                      (fn []
                                                        (awful.client.focus.byidx 1))
                                                      {:description "focus next by index"
                                                       :group :client})
                                           (awful.key [modkey] :k
                                                      (fn []
                                                        (awful.client.focus.byidx (- 1)))
                                                      {:description "focus previous by index"
                                                       :group :client})
                                           (awful.key [modkey] :Tab
                                                      (fn []
                                                        (awful.client.focus.history.previous)
                                                        (when client.focus
                                                          (client.focus:raise)))
                                                      {:description "go back"
                                                       :group :client})
                                           (awful.key [modkey :Control] :j
                                                      (fn []
                                                        (awful.screen.focus_relative 1))
                                                      {:description "focus the next screen"
                                                       :group :screen})
                                           (awful.key [modkey :Control] :k
                                                      (fn []
                                                        (awful.screen.focus_relative (- 1)))
                                                      {:description "focus the previous screen"
                                                       :group :screen})
                                           (awful.key [modkey :Control] :n
                                                      (fn []
                                                        (let [c (awful.client.restore)]
                                                          (when c
                                                            (c:activate {:context :key.unminimize
                                                                         :raise true}))))
                                                      {:description "restore minimized"
                                                       :group :client})])

(awful.keyboard.append_global_keybindings [(awful.key [modkey :Shift] :j
                                                      (fn []
                                                        (awful.client.swap.byidx 1))
                                                      {:description "swap with next client by index"
                                                       :group :client})
                                           (awful.key [modkey :Shift] :k
                                                      (fn []
                                                        (awful.client.swap.byidx (- 1)))
                                                      {:description "swap with previous client by index"
                                                       :group :client})
                                           (awful.key [modkey] :u
                                                      awful.client.urgent.jumpto
                                                      {:description "jump to urgent client"
                                                       :group :client})
                                           (awful.key [modkey] :l
                                                      (fn []
                                                        (awful.tag.incmwfact 0.05))
                                                      {:description "increase master width factor"
                                                       :group :layout})
                                           (awful.key [modkey] :h
                                                      (fn []
                                                        (awful.tag.incmwfact (- 0.05)))
                                                      {:description "decrease master width factor"
                                                       :group :layout})
                                           (awful.key [modkey :Shift] :h
                                                      (fn []
                                                        (awful.tag.incnmaster 1
                                                                              nil
                                                                              true))
                                                      {:description "increase the number of master clients"
                                                       :group :layout})
                                           (awful.key [modkey :Shift] :l
                                                      (fn []
                                                        (awful.tag.incnmaster (- 1)
                                                                              nil
                                                                              true))
                                                      {:description "decrease the number of master clients"
                                                       :group :layout})
                                           (awful.key [modkey :Control] :h
                                                      (fn []
                                                        (awful.tag.incncol 1
                                                                           nil
                                                                           true))
                                                      {:description "increase the number of columns"
                                                       :group :layout})
                                           (awful.key [modkey :Control] :l
                                                      (fn []
                                                        (awful.tag.incncol (- 1)
                                                                           nil
                                                                           true))
                                                      {:description "decrease the number of columns"
                                                       :group :layout})
                                           (awful.key [modkey] :space
                                                      (fn []
                                                        (awful.layout.inc 1))
                                                      {:description "select next"
                                                       :group :layout})
                                           (awful.key [modkey :Shift] :space
                                                      (fn []
                                                        (awful.layout.inc (- 1)))
                                                      {:description "select previous"
                                                       :group :layout})])

(awful.keyboard.append_global_keybindings [(awful.key {:description "only view tag"
                                                       :group :tag
                                                       :keygroup :numrow
                                                       :modifiers [modkey]
                                                       :on_press (fn [index]
                                                                   (local screen
                                                                          (awful.screen.focused))
                                                                   (local tag
                                                                          (. screen.tags
                                                                             index))
                                                                   (when tag
                                                                     (tag:view_only)))})
                                           (awful.key {:description "toggle tag"
                                                       :group :tag
                                                       :keygroup :numrow
                                                       :modifiers [modkey
                                                                   :Control]
                                                       :on_press (fn [index]
                                                                   (local screen
                                                                          (awful.screen.focused))
                                                                   (local tag
                                                                          (. screen.tags
                                                                             index))
                                                                   (when tag
                                                                     (awful.tag.viewtoggle tag)))})
                                           (awful.key {:description "move focused client to tag"
                                                       :group :tag
                                                       :keygroup :numrow
                                                       :modifiers [modkey
                                                                   :Shift]
                                                       :on_press (fn [index]
                                                                   (when client.focus
                                                                     (local tag
                                                                            (. client.focus.screen.tags
                                                                               index))
                                                                     (when tag
                                                                       (client.focus:move_to_tag tag))))})
                                           (awful.key {:description "toggle focused client on tag"
                                                       :group :tag
                                                       :keygroup :numrow
                                                       :modifiers [modkey
                                                                   :Control
                                                                   :Shift]
                                                       :on_press (fn [index]
                                                                   (when client.focus
                                                                     (local tag
                                                                            (. client.focus.screen.tags
                                                                               index))
                                                                     (when tag
                                                                       (client.focus:toggle_tag tag))))})
                                           (awful.key {:description "select layout directly"
                                                       :group :layout
                                                       :keygroup :numpad
                                                       :modifiers [modkey]
                                                       :on_press (fn [index]
                                                                   (local t
                                                                          (. (awful.screen.focused)
                                                                             :selected_tag))
                                                                   (when t
                                                                     (set t.layout
                                                                          (or (. t.layouts
                                                                                 index)
                                                                              t.layout))))})])

(client.connect_signal "request::default_mousebindings"
                       (fn []
                         (awful.mouse.append_client_mousebindings [(awful.button {}
                                                                                 1
                                                                                 (fn [c]
                                                                                   (c:activate {:context :mouse_click})))
                                                                   (awful.button [modkey]
                                                                                 1
                                                                                 (fn [c]
                                                                                   (c:activate {:action :mouse_move
                                                                                                :context :mouse_click})))
                                                                   (awful.button [modkey]
                                                                                 3
                                                                                 (fn [c]
                                                                                   (c:activate {:action :mouse_resize
                                                                                                :context :mouse_click})))])))

(client.connect_signal "request::default_keybindings"
                       (fn []
                         (awful.keyboard.append_client_keybindings [(awful.key [modkey]
                                                                               :f
                                                                               (fn [c]
                                                                                 (set c.fullscreen
                                                                                      (not c.fullscreen))
                                                                                 (c:raise))
                                                                               {:description "toggle fullscreen"
                                                                                :group :client})
                                                                    (awful.key [modkey]
                                                                               :w
                                                                               (fn [c]
                                                                                 (c:kill))
                                                                               {:description :close
                                                                                :group :client})
                                                                    (awful.key [modkey
                                                                                :Control]
                                                                               :f
                                                                               awful.client.floating.toggle
                                                                               {:description "toggle floating"
                                                                                :group :client})
                                                                    (awful.key [modkey
                                                                                :Control]
                                                                               :Return
                                                                               (fn [c]
                                                                                 (c:swap (awful.client.getmaster)))
                                                                               {:description "move to master"
                                                                                :group :client})
                                                                    (awful.key [modkey]
                                                                               :o
                                                                               (fn [c]
                                                                                 (c:move_to_screen))
                                                                               {:description "move to screen"
                                                                                :group :client})
                                                                    (awful.key [modkey]
                                                                               :t
                                                                               (fn [c]
                                                                                 (set c.ontop
                                                                                      (not c.ontop)))
                                                                               {:description "toggle keep on top"
                                                                                :group :client})
                                                                    (awful.key [modkey]
                                                                               :n
                                                                               (fn [c]
                                                                                 (set c.minimized
                                                                                      true))
                                                                               {:description :minimize
                                                                                :group :client})
                                                                    (awful.key [modkey]
                                                                               :m
                                                                               (fn [c]
                                                                                 (set c.maximized
                                                                                      (not c.maximized))
                                                                                 (c:raise))
                                                                               {:description "(un)maximize"
                                                                                :group :client})
                                                                    (awful.key [modkey
                                                                                :Control]
                                                                               :m
                                                                               (fn [c]
                                                                                 (set c.maximized_vertical
                                                                                      (not c.maximized_vertical))
                                                                                 (c:raise))
                                                                               {:description "(un)maximize vertically"
                                                                                :group :client})
                                                                    (awful.key [modkey
                                                                                :Shift]
                                                                               :m
                                                                               (fn [c]
                                                                                 (set c.maximized_horizontal
                                                                                      (not c.maximized_horizontal))
                                                                                 (c:raise))
                                                                               {:description "(un)maximize horizontally"
                                                                                :group :client})])))

(ruled.client.connect_signal "request::rules"
                             (fn []
                               (ruled.client.append_rule {:id :global
                                                          :properties {:focus awful.client.focus.filter
                                                                       :placement (+ awful.placement.no_overlap
                                                                                     awful.placement.no_offscreen)
                                                                       :raise true
                                                                       :screen awful.screen.preferred}
                                                          :rule {}})
                               (ruled.client.append_rule {:id :floating
                                                          :properties {:floating true}
                                                          :rule_any {:class [:Arandr
                                                                             :Blueman-manager
                                                                             :Gpick
                                                                             :Kruler
                                                                             :Sxiv
                                                                             "Tor Browser"
                                                                             :Wpa_gui
                                                                             :veromix
                                                                             :xtightvncviewer]
                                                                     :instance [:copyq
                                                                                :pinentry]
                                                                     :name ["Event Tester"]
                                                                     :role [:AlarmWindow
                                                                            :ConfigManager
                                                                            :pop-up]}})
                               (ruled.client.append_rule {:id :titlebars
                                                          :properties {:titlebars_enabled true}
                                                          :rule_any {:type [:normal
                                                                            :dialog]}})
                               (ruled.client.append_rule {:properties {:maximized true
                                                                       :screen 1
                                                                       :tag :1}
                                                          :rule {:class :Firefox}})
                               (ruled.client.append_rule {:properties {:maximized true
                                                                       :screen 1
                                                                       :tag :2}
                                                          :rule {:class :Emacs}})
                               (ruled.client.append_rule {:properties {:maximized true
                                                                       :screen 1
                                                                       :tag :3}
                                                          :rule {:class :kitty}})
                               (ruled.client.append_rule {:properties {:screen 1
                                                                       :tag :4}
                                                          :rule {:class :Telegram}})
                               (ruled.client.append_rule {:properties {:screen 1
                                                                       :tag :5}
                                                          :rule {:class :Spotify}})))

(client.connect_signal "request::titlebars"
                       (fn [c]
                         (let [buttons [(awful.button {} 1
                                                      (fn []
                                                        (c:activate {:action :mouse_move
                                                                     :context :titlebar})))
                                        (awful.button {} 3
                                                      (fn []
                                                        (c:activate {:action :mouse_resize
                                                                     :context :titlebar})))]]
                           (tset (awful.titlebar c) :widget
                                 {1 {1 (awful.titlebar.widget.iconwidget c)
                                     : buttons
                                     :layout wibox.layout.fixed.horizontal}
                                  2 {1 {:halign :center
                                        :widget (awful.titlebar.widget.titlewidget c)}
                                     : buttons
                                     :layout wibox.layout.flex.horizontal}
                                  3 {1 (awful.titlebar.widget.floatingbutton c)
                                     2 (awful.titlebar.widget.maximizedbutton c)
                                     3 (awful.titlebar.widget.stickybutton c)
                                     4 (awful.titlebar.widget.ontopbutton c)
                                     5 (awful.titlebar.widget.closebutton c)
                                     :layout (wibox.layout.fixed.horizontal)}
                                  :layout wibox.layout.align.horizontal}))))

(ruled.notification.connect_signal "request::rules"
                                   (fn []
                                     (ruled.notification.append_rule {:properties {:implicit_timeout 5
                                                                                   :screen awful.screen.preferred}
                                                                      :rule {}})))

(naughty.connect_signal "request::display"
                        (fn [n] (naughty.layout.box {:notification n})))

(client.connect_signal "mouse::enter"
                       (fn [c]
                         (c:activate {:context :mouse_enter :raise false})))
