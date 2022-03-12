from typing import List  # noqa: F401

from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
import os
import subprocess

mod = "mod4"
terminal = guess_terminal("alacritty")

def latest_group(qtile):
    qtile.current_screen.set_group(qtile.current_screen.previous_group)

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    # Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    # Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "e", lazy.spawn("emacsclient -c -a 'emacs'"), desc="Launch emacsclient"),
    Key([mod], "z", lazy.function(latest_group)),

    Key([mod], "h", lazy.layout.shrink_main()),
    Key([mod], "l", lazy.layout.grow_main()),
    Key([mod], "j", lazy.layout.next(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.previous(), desc="Move focus up"),
    Key([mod], "space", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "m", lazy.layout.swap_main(), desc="Move window to main between layouts"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    #Key(
    #    [mod, "shift"],
    #    "Return",
    #    lazy.layout.toggle_split(),
    #    desc="Toggle between split and unsplit sides of stack",
    #),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([mod], "p", lazy.spawn("dmenu_run -fn 'SFThonburi-11'"), desc="Spawn a command using a prompt widget"),
]

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

# Catpuccin
colors = [
   ["#1a1823", "#1a1823"],  # 0 background
   ["#6e6c7e", "#6e6c7e"],  # 1 foreground
   ["#302d42", "#302d42"],  # 2 background lighter
   ["#f28fad", "#f28fad"],  # 3 red
   ["#abe9b3", "#abe9b3"],  # 4 green
   ["#fae3b0", "#fae3b0"],  # 5 yellow
   ["#96cdfb", "#96cdfb"],  # 6 blue
   ["#e8a2af", "#e8a2af"],  # 7 maroon
   ["#89dceb", "#89dceb"],  # 8 cyan
   ["#c3bac6", "#c3bac6"],  # 9 grey
   ["#d9e0ee", "#d9e0ee"],  # 10 white
   ["#f8bd96", "#f8bd96"],  # 11 orange
   ["#8fbcbb", "#8fbcbb"],  # 12 super cyan
   ["#c9cbff", "#c9cbff"],  # 13 super blue
   ["#131020", "#131020"],  # 14 super dark background
   ["#988ba2", "#988ba2"]   # 15 slate grey
]

layout_theme = {"border_width": 2,
                "border_focus": colors[9],
                "border_normal": colors[0]
                }

layouts = [
    layout.MonadTall(**layout_theme),
    layout.MonadWide(**layout_theme),
    layout.Max(**layout_theme),
    layout.RatioTile(**layout_theme),
    layout.Zoomy(**layout_theme),
    # layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4),
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
]

widget_defaults = dict(
    font="SFThonburi",
    fontsize=28,
    padding=4,
    foreground=colors[15],
    # background=colors[0],
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(),
                widget.TextBox("|"),
                widget.CurrentLayout(),
                widget.TextBox("|"),
                widget.WindowCount(show_zero=True),
                widget.TextBox("|"),
                widget.WindowName(),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.Clock(format="%a %b %d %H:%M"),
                widget.Systray(icon_size=38),
            ],
            48,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.run([home])
