hl.monitor({
    output   = "eDP-1",
    mode     = "preferred",
    position = "auto",
    scale    = 1,
})

hl.monitor({
    output   = "",
    mode     = "preferred",
    position = "auto",
    scale    = 1,
    mirror   = "eDP-1",
})

local terminal    = "kitty"
local fileManager = "nautilus"
local menu        = "wofi --show drun --style ~/.config/wofi/style.css --allow-images"

hl.on("hyprland.start", function()
    hl.exec_cmd("hyprpaper")
    hl.exec_cmd("mako")
    hl.exec_cmd("waybar")
    hl.exec_cmd("hypridle")
    hl.exec_cmd("udiskie -a")
end)

hl.env("QT_QPA_PLATFORM", "wayland;xcb")
hl.env("QT_QPA_PLATFORMTHEME", "qt6ct")
hl.env("QT_WAYLAND_DISABLE_WINDOWDECORATION", "1")
hl.env("QT_AUTO_SCREEN_SCALE_FACTOR", "1")
hl.env("QT_STYLE_OVERRIDE", "kvantum")

hl.env("GDK_SCALE", "1")
hl.env("GDK_BACKEND", "wayland,x11,*")
hl.env("SDL_VIDEODRIVER", "wayland")
hl.env("CLUTTER_BACKEND", "wayland")

hl.env("XDG_CURRENT_DESKTOP", "Hyprland")
hl.env("XDG_SESSION_TYPE", "wayland")
hl.env("XDG_SESSION_DESKTOP", "Hyprland")

hl.env("HYPRCURSOR_THEME", "rose-pine-hyprcursor")
hl.env("HYPRCURSOR_SIZE", "26")
hl.env("XCURSOR_SIZE", "12")

hl.config({
    general = {
        gaps_in          = 2,
        gaps_out         = 2,
        border_size      = 1,
        resize_on_border = true,
        allow_tearing    = false,
        layout           = "dwindle",
        col = {
            -- active_border = { colors = { "rgba(33ccffee)", "rgba(00f090ee)" }, angle = 45 },
            active_border   = "rgba(33ccffee)",
            inactive_border = "rgba(595959ee)",
        },
    },

    decoration = {
        rounding = 5,
        -- active_opacity   = 0.9,
        -- inactive_opacity = 0.8,

        -- shadow = {
        --     enabled      = false,
        --     range        = 4,
        --     render_power = 3,
        --     color        = "rgba(1a1a1aee)",
        -- },

        -- blur = {
        --     enabled = true,
        --     size    = 8,
        --     passes  = 1,
        -- },
    },

    animations = {
        enabled = false,
    },

    misc = {
        force_default_wallpaper  = 0,
        disable_splash_rendering = false,
    },

    -- xwayland = {
    --     force_zero_scaling = true,
    -- },
})

hl.curve("myBezier", { type = "bezier", points = { {1, 0.9}, {0.1, 1} } })

hl.animation({ leaf = "windows",     enabled = true, speed = 2, bezier = "myBezier" })
hl.animation({ leaf = "windowsOut",  enabled = true, speed = 2, bezier = "default", style = "popin 80%" })
hl.animation({ leaf = "border",      enabled = true, speed = 3, bezier = "default" })
hl.animation({ leaf = "borderangle", enabled = true, speed = 2, bezier = "default" })
hl.animation({ leaf = "fade",        enabled = true, speed = 2, bezier = "default" })
hl.animation({ leaf = "workspaces",  enabled = true, speed = 1, bezier = "default" })

hl.config({
    input = {
        kb_layout    = "de",
        kb_variant   = "nodeadkeys",
        kb_model     = "",
        kb_options   = "",
        kb_rules     = "",
        follow_mouse = 2,
        sensitivity  = 0,
        touchpad = {
            natural_scroll = true,
        },
    },
})

hl.gesture({
    fingers   = 3,
    direction = "horizontal",
    action    = "workspace",
})

---------------------
---- KEYBINDINGS ----
---------------------

local mainMod = "SUPER"

hl.bind(mainMod .. " + Q", hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + C", hl.dsp.window.close())
hl.bind(mainMod .. " + M", hl.dsp.exec_cmd("command -v hyprshutdown >/dev/null 2>&1 && hyprshutdown || hyprctl dispatch 'hl.dsp.exit()'"))
hl.bind(mainMod .. " + E", hl.dsp.exec_cmd(fileManager))
hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + R", hl.dsp.exec_cmd(menu))
hl.bind(mainMod .. " + F", hl.dsp.exec_cmd("firefox"))
hl.bind(mainMod .. " + G", hl.dsp.exec_cmd("emacsclient -c"))
hl.bind(mainMod .. " + S", hl.dsp.exec_cmd('wf-recorder -g "$(slurp)" -f /home/julian/Videos/screen-recording.mp4'))

hl.bind(mainMod .. " + left",  hl.dsp.focus({ direction = "l" }))
hl.bind(mainMod .. " + right", hl.dsp.focus({ direction = "r" }))
hl.bind(mainMod .. " + up",    hl.dsp.focus({ direction = "u" }))
hl.bind(mainMod .. " + down",  hl.dsp.focus({ direction = "d" }))

hl.bind(mainMod .. " + SHIFT + left",  hl.dsp.window.move({ direction = "l" }))
hl.bind(mainMod .. " + SHIFT + right", hl.dsp.window.move({ direction = "r" }))
hl.bind(mainMod .. " + SHIFT + up",    hl.dsp.window.move({ direction = "u" }))
hl.bind(mainMod .. " + SHIFT + down",  hl.dsp.window.move({ direction = "d" }))

-- 10 maps to key 0
for i = 1, 10 do
    local key = i % 10
    hl.bind(mainMod .. " + " .. key,         hl.dsp.focus({ workspace = i }))
    hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

hl.bind(mainMod .. " + CTRL + up",    hl.dsp.window.resize({ x =   0, y = -10, relative = true }), { repeating = true })
hl.bind(mainMod .. " + CTRL + down",  hl.dsp.window.resize({ x =   0, y =  10, relative = true }), { repeating = true })
hl.bind(mainMod .. " + CTRL + left",  hl.dsp.window.resize({ x = -10, y =   0, relative = true }), { repeating = true })
hl.bind(mainMod .. " + CTRL + right", hl.dsp.window.resize({ x =  10, y =   0, relative = true }), { repeating = true })

-- hl.bind(mainMod .. " + S",         hl.dsp.workspace.toggle_special("magic"))
-- hl.bind(mainMod .. " + SHIFT + S", hl.dsp.window.move({ workspace = "special:magic" }))

hl.bind("F11", hl.dsp.window.fullscreen())

hl.bind(mainMod .. " + mouse:272",         hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + SHIFT + mouse:272", hl.dsp.window.resize(), { mouse = true })

hl.bind("XF86AudioMute",        hl.dsp.exec_cmd("pactl set-sink-mute @DEFAULT_SINK@ toggle"),        { locked = true })
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),        { locked = true, repeating = true })

hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("brightnessctl set +5%"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl set 5%-"), { locked = true, repeating = true })

hl.bind(mainMod .. " + SHIFT + C", hl.dsp.exec_cmd("hyprpicker -a --format=hex"))
hl.bind(mainMod .. " + L",         hl.dsp.exec_cmd("hyprlock"))
hl.bind("XF86PowerOff",            hl.dsp.exec_cmd("hypridle"))

hl.bind(mainMod .. " + SHIFT + S",        hl.dsp.exec_cmd('grim -g "$(slurp)" - | wl-copy'))
hl.bind(mainMod .. " + CTRL + SHIFT + S", hl.dsp.exec_cmd("grim - | wl-copy"))

hl.bind(mainMod .. " + Escape", hl.dsp.exec_cmd("wlogout"))
