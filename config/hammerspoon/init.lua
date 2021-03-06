hs.hotkey.bindSpec({{"cmd", "ctrl"}, "e"},
    function ()
        hs.task.new("/bin/bash", nil, {"-l", "-c", "emacsclient --eval '(emacs-everywhere)'"}):start()
    end
)
