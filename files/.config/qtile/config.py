import itertools
from libqtile.config import (
    Group,
    Match,
    Screen,
    EzKey as Key,
    EzDrag as Drag,
    EzClick as Click,
)
from libqtile.command import lazy
from libqtile.widget import base as widget_base
from libqtile import layout, bar, widget, hook
import os
import os.path
from datetime import datetime
from functools import partial
import subprocess
import shlex
import pyudev
import gi
from pulsectl import Pulse

gi.require_version("Gtk", "3.0")
from gi.repository import Gtk, Gdk, Notify


# TODO: fix fullscreen
# TODO: MTP
# TODO: no skype in systray

Notify.init("qtile")

terminal_program = os.environ.get("TERMINAL", "xfce4-terminal")


def notify(title, content):
    Notify.Notification.new(title, content).show()


class MyPrompt(widget.Prompt):
    pass


class ProcessTrackerWidget2(widget_base.ThreadPoolText):
    orientations = widget_base.ORIENTATION_HORIZONTAL
    defaults = [
        ("name", "NAME", "Title of widget"),
        ("cmd_start", "CMD", "Command to run"),
        ("cmd_stop", "CMD_STOP", "Command to run to stop"),
        ("cmd_status", "CMD_STATUS", "Command to run to check status"),
    ]

    def __init__(self, **config):
        super().__init__(text="", markup=True, **config)
        self.add_defaults(ProcessTrackerWidget2.defaults)
        self.running = True

    def poll(self):
        ret_code = subprocess.call(shlex.split(self.cmd_status))
        self.running = ret_code == 0
        return self._get_text()

    def _get_text(self):
        if self.running:
            color = "green"
        else:
            color = "grey"

        return '<span color="{color}">{name}</span>'.format(color=color, name=self.name)

    def _refresh(self):
        self.update(self.poll())

    def button_press(self, x, y, button):
        if self.running:
            cmd = self.cmd_stop
        else:
            cmd = self.cmd_start

        fut = self.qtile.run_in_executor(
            partial(subprocess.check_call, shlex.split(cmd))
        )
        fut.add_done_callback(lambda fut: self._refresh())
        self.timer_setup()


class ProcessTrackerWidget(widget_base._TextBox):
    orientations = widget_base.ORIENTATION_HORIZONTAL
    defaults = [
        ("name", "NAME", "Title of widget"),
        ("cmd", "CMD", "Command to run"),
        ("disable_cmd", False, "Command to run instead of SIGTERM"),
    ]

    def __init__(self, **config):
        super().__init__(text="", markup=True, **config)
        self.proc = None
        self.add_defaults(ProcessTrackerWidget.defaults)
        self._set_text()

    def _set_text(self):
        if self.proc:
            color = "green"
        else:
            color = "grey"

        self.text = '<span color="{color}">{name}</span>'.format(
            color=color, name=self.name
        )
        self.draw()

    def button_press(self, x, y, btn):
        print("button_press")
        if not self.proc:
            self.proc = subprocess.Popen(shlex.split(self.cmd))
            try:
                retval = self.proc.wait(timeout=1)
            except subprocess.TimeoutExpired:
                pass
            else:
                self.proc = None
        else:
            if self.disable_cmd:
                subprocess.check_call(shlex.split(self.disable_cmd))
                self.proc.wait()
            else:
                self.proc.terminate()
                self.proc.wait()
            self.proc = None

        self._set_text()


class ImprovedTaskListWidget(widget.TaskList):
    def box_width(self, text):
        num_of_windows = len(self.bar.screen.group.windows)
        max_width = self.width
        return max_width / num_of_windows


class DropdownWidget(widget_base._TextBox):
    orientations = widget_base.ORIENTATION_HORIZONTAL
    css = b"""
        row {
            background-color: #222222;
        }

        row:hover {
            background-color: #215578;
        }

        label {
            color: white;
        }

        label:hover {
            color: black;
        }
    """

    def __init__(self, **config):
        super().__init__(text="PM", **config)
        style_provider = Gtk.CssProvider()
        style_provider.load_from_data(self.css)
        Gtk.StyleContext.add_provider_for_screen(
            Gdk.Screen.get_default(),
            style_provider,
            Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION,
        )

        self._win = Gtk.Window(name="notification", type=Gtk.WindowType.POPUP)
        self._win.connect("delete-event", Gtk.main_quit)

        self._listbox = Gtk.ListBox()
        self._listbox.set_selection_mode(Gtk.SelectionMode.NONE)
        self._win.add(self._listbox)
        self.add_entry(
            "Log Off", "loginctl kill-session " + os.getenv("XDG_SESSION_ID")
        )
        self.add_entry("Suspend", "systemctl suspend")
        self.add_entry("Hibernate", "systemctl hibernate")
        self.add_entry("Restart", "systemctl reboot")
        self.add_entry("Shutdown", "systemctl poweroff")
        self._win.move(-2000, -2000)
        self._win.show_all()
        self._visible = False

    def add_entry(self, text, spawn_cmd):
        def action(arg1, arg2):
            subprocess.check_call(shlex.split(spawn_cmd))

        row = Gtk.ListBoxRow()
        hbox = Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=50)
        row.add(hbox)
        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL)
        hbox.pack_start(vbox, True, True, 0)
        ev_box = Gtk.EventBox()
        label1 = Gtk.Label(text, xalign=0)
        ev_box.add(label1)
        ev_box.connect("button-press-event", action)
        vbox.pack_start(ev_box, True, True, 0)
        self._listbox.add(row)

    def button_press(self, x, y, btn):
        from gi.repository import GLib

        GLib.idle_add(self._button_press, x, y, btn)

    def _button_press(self, x, y, btn):
        win_size = self._win.get_size()
        win_x = self.offset + self.width - win_size[0]
        win_y = self.bar.height
        self._win.move(win_x, win_y)
        if self._visible:
            self._win.hide()
        else:
            self._win.show_all()

        self._visible = not self._visible
        # need to return false in order to not be called again
        # see http://www.pygtk.org/docs/pygobject/glib-functions.html#function-glib--idle-add
        return False


class RFKillWidget(widget_base._TextBox):
    DEV_NAME_MAP = {"WLAN": "ieee80211", "BT": "bluetooth"}
    DEV_NAME_ID_MAP = {"WLAN": "wlan", "BT": "bluetooth"}

    orientations = widget_base.ORIENTATION_HORIZONTAL
    defaults = [("device_name", "device", "Name to use for device (e.g. BT, WLAN)")]

    def __init__(self, **config):
        super().__init__(markup=True, **config)
        self.add_defaults(RFKillWidget.defaults)
        if self.device_name not in ["WLAN", "BT"]:
            raise Exception("Invalid device_name")

        ctx = pyudev.Context()
        rfkill_paths = [
            "/sys/class/rfkill/" + os.readlink("/sys/class/rfkill/" + name)
            for name in os.listdir("/sys/class/rfkill")
        ]
        print(rfkill_paths)
        devices = [
            pyudev.Devices.from_path(ctx, os.path.abspath(path))
            for path in rfkill_paths
            if self.DEV_NAME_MAP[self.device_name] in path
        ]

        if len(devices) != 1:
            raise Exception("no or too many devices")

        self._dev = devices[0]
        monitor = pyudev.Monitor.from_netlink(ctx)
        monitor.filter_by("rfkill")
        observer = pyudev.MonitorObserver(monitor, self._udev_callback)
        observer.start()
        self.text = self._get_text()

    def _udev_callback(self, action, dev):
        if dev == self._dev:
            # new dev has new attributes
            self._dev = dev
            self.text = self._get_text()
            self.bar.draw()

    def is_blocked(self):
        return bool(int(self._dev.attributes.get("soft")))

    def _get_text(self):
        if self.is_blocked():
            color = "gray"
        else:
            color = "green"

        return '<span color="{color}">{name}</span>'.format(
            color=color, name=self.device_name
        )

    def button_press(self, event, x, y):
        subprocess.check_call(
            [
                "rfkill",
                "unblock" if self.is_blocked() else "block",
                self.DEV_NAME_ID_MAP[self.device_name],
            ]
        )


widget_defaults = {
    "font": "Monospace",
    "fontsize": 15,
    "padding": 3,
    "inactive": "#aaaaaa",
}

groups = [
    Group("MAIN"),
    Group("WORK", matches=[Match(wm_class=["Emacs"])]),
    Group("COMMS", matches=[Match(wm_class=["skypeforlinux", "Slack", "Thunderbird"])]),
    Group("OTHER"),
    Group("MUSIC", matches=[Match(wm_class=["Clementine"])]),
]

dgroups_key_binder = None

layouts = [
    layout.columns.Columns(
        border_normal="#000000",
        border_focus="#ff4d96",
        border_width=1,
        border_normal_stack="#000000",
        border_focus_stack="#535d6c",
        fair=False,
        columns=2,
        name="col",
    ),
    layout.Max(),
]

screens = []


def switch_kbd_layout(qtile):
    widgets = None
    if hasattr(qtile, "widgetMap"):
        # qtile < 0.14.0
        widgets = qtile.widgetMap
    elif hasattr(qtile, "widgets_map"):
        # qtile >= 0.14.0
        widgets = qtile.widgets_map
    else:
        raise Exception("cannot find qtile widgets map")

    widget = widgets["keyboardlayout"]
    widget.next_keyboard()


def switch_pulse_default(prop):
    pulse = Pulse("qtile")
    current_default = getattr(pulse.server_info(), "default_{}_name".format(prop))

    set_next = False
    things = getattr(pulse, "{}_list".format(prop))()
    for thing in itertools.chain(things, things):
        if set_next:
            notify("Sound", "Default {} to {}".format(prop, thing.name))
            pulse.default_set(thing)
            break
        elif thing.name == current_default:
            set_next = True

    if prop == "sink":
        list_cmd = "list-sink-inputs"
        move_cmd = "move-sink-input"
    else:
        list_cmd = "list-source-outputs"
        move_cmd = "move-source-output"

    streams = subprocess.getoutput(
        "pacmd {} | grep index | awk '{{ print $2 }}'".format(list_cmd)
    )
    streams = [int(idx.strip()) for idx in streams.splitlines()]

    for stream in streams:
        cmd = "pacmd {cmd} {stream_id} {thing_id}".format(
            cmd=move_cmd, stream_id=stream, thing_id=thing.index
        )
        print(cmd)
        subprocess.check_call(shlex.split(cmd))


def switch_pulse_default_sink(qtile):
    return switch_pulse_default("sink")


def switch_pulse_default_source(qtile):
    return switch_pulse_default("source")


keys = [
    Key("M-p", lazy.layout.up()),
    Key("M-f", lazy.layout.right()),
    Key("M-n", lazy.layout.down()),
    Key("M-v", lazy.window.toggle_minimize()),
    Key("M-b", lazy.layout.left()),
    Key("M-C-p", lazy.layout.shuffle_up()),
    Key("M-C-f", lazy.layout.shuffle_right()),
    Key("M-C-n", lazy.layout.shuffle_down()),
    Key("M-C-b", lazy.layout.shuffle_left()),
    Key("M-S-f", lazy.layout.grow_right()),
    Key("M-S-b", lazy.layout.grow_left()),
    Key("M-<space>", lazy.layout.next()),
    Key("M-S-<space>", lazy.layout.rotate()),
    Key("M-C-<Return>", lazy.layout.toggle_split()),
    Key("M-m", lazy.layout.toggle_fullscreen()),
    Key("M-S-<Return>", lazy.window.toggle_floating()),
    Key("C-S-<grave>", lazy.spawn(terminal_program)),
    Key("M-<Tab>", lazy.next_layout()),
    Key("M-o", lazy.next_screen()),
    Key("M-q", lazy.window.kill()),
    Key("M-C-r", lazy.restart()),
    Key("M-C-q", lazy.shutdown()),
    Key("M-r", lazy.spawncmd()),
    Key("A-<Shift_L>", lazy.function(switch_kbd_layout)),
    Key("<XF86AudioRaiseVolume>", lazy.spawn("amixer -q sset Master 5%+")),
    Key("<XF86AudioLowerVolume>", lazy.spawn("amixer -q sset Master 5%-")),
    Key("A-<XF86AudioRaiseVolume>", lazy.spawn("amixer -q sset Master 100%")),
    Key("A-<XF86AudioLowerVolume>", lazy.spawn("amixer -q sset Master 0%")),
    Key("<XF86AudioMute>", lazy.spawn("amixer -q sset Master toggle")),
    Key("M-<bracketleft>", lazy.function(switch_pulse_default_sink)),
    Key("M-<bracketright>", lazy.function(switch_pulse_default_source)),
    Key("<XF86MonBrightnessUp>", lazy.spawn("light -A 10")),
    Key("<XF86MonBrightnessDown>", lazy.spawn("light -U 10")),
    Key("<XF86KbdBrightnessUp>", lazy.spawn("kbdlight up")),
    Key("<XF86KbdBrightnessDown>", lazy.spawn("kbdlight down")),
    Key("A-<XF86KbdBrightnessUp>", lazy.spawn("kbdlight max")),
    Key("A-<XF86KbdBrightnessDown>", lazy.spawn("kbdlight off")),
    Key("A-<XF86MonBrightnessUp>", lazy.spawn("light -S 100")),
    Key("A-<XF86MonBrightnessDown>", lazy.spawn("light -S 0")),
    Key("M-C-h", lazy.spawn("nautilus")),
    Key("M-C-e", lazy.spawn("emacs")),
    Key("M-C-c", lazy.spawn("gnome-calculator")),
    Key("M-C-l", lazy.spawn("physlock -s")),
]

keys.extend(
    Key("M-" + str(idx), lazy.group[group.name].toscreen())
    for idx, group in enumerate(groups, start=1)
)

keys.extend(
    Key("M-S-" + str(idx), lazy.window.togroup(group.name))
    for idx, group in enumerate(groups, start=1)
)

mouse = [
    Drag(
        "M-<Button1>",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag("M-<Button3>", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click("M-<Button2>", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating()
auto_fullscreen = True


@hook.subscribe.startup
def setwallpaper():
    subprocess.check_call(
        shlex.split("feh --bg-scale Pictures/wallpaper1 Pictures/wallpaper2")
    )


@hook.subscribe.startup_once
def autostart():
    # subprocess.call(["systemctl", "--user", "import-environment"])
    services = [
        "monitor-set-initial-mode",
        "clementine",
        "emacs",
        "emacs-client-frame",
        "firefox",
        "keyboard",
        "nm-applet",
        "thunderbird",
        "compton",
        "blueman-applet",
    ]

    now = datetime.now()
    if now.weekday() < 5 and now.hour < 19 and now.hour > 8:
        services.append("slack")

    for service in services:
        subprocess.call(["systemctl", "--user", "start", service])


def setup_screens(qtile):
    bar_commons = {"size": 26, "background": "#222222"}

    screens_widgets = {
        "group-box": partial(widget.GroupBox, rounded=False, highlight_method="block"),
        "prompt": partial(widget.Prompt),
        "task-list": partial(
            ImprovedTaskListWidget, rounded=False, highlight_method="block"
        ),
        "systray": partial(widget.Systray),
        "clock": partial(widget.Clock, format="%a %d %b, %H:%M"),
        "sep": partial(widget.Sep),
        # Commented out because it degrading performance.
        # See: https://github.com/qtile/qtile/issues/1446
        # 'kbd-layout': partial(widget.KeyboardLayout, configured_keyboards=['us', 'el,us']),
        "layout": partial(widget.CurrentLayout),
        "power-management": partial(DropdownWidget),
    }

    num_of_screens = len(qtile.conn.pseudoscreens)
    rfkill_widgets = []
    for device in ["WLAN", "BT"]:
        try:
            rfkill_widgets.append(RFKillWidget(device_name=device))
        except Exception:
            pass

    main_screen_widgets = (
        [
            screens_widgets["group-box"](),
            screens_widgets["prompt"](),
            screens_widgets["task-list"](),
            screens_widgets["systray"](),
            screens_widgets["clock"](),
            screens_widgets["sep"](),
        ]
        + rfkill_widgets
        + [
            ProcessTrackerWidget(
                name="MTP",
                cmd="go-mtpfs mnt/android-mtp",
                disable_cmd="fusermount -u mnt/android-mtp",
            ),
            ProcessTrackerWidget2(
                name="TLP",
                cmd_status="systemctl status tlp",
                cmd_start="gksu systemctl start tlp",
                cmd_stop="gksu systemctl stop tlp",
                update_interval=15,
            ),
            screens_widgets["sep"](),
            # screens_widgets['kbd-layout'](),
            screens_widgets["sep"](),
            widget.Battery(
                charge_char='<span color="green">BAT</span>',
                discharge_char='<span color="orange">BAT</span>',
                low_percentage=0.2,
                format="{char} {percent:2.0%}",
                markup=True,
            ),
            screens_widgets["sep"](),
            screens_widgets["layout"](),
            screens_widgets["sep"](),
            screens_widgets["power-management"](),
        ]
    )

    secondary_screen_widgets = [
        screens_widgets["group-box"](),
        screens_widgets["prompt"](),
        screens_widgets["task-list"](),
        screens_widgets["clock"](),
        screens_widgets["sep"](),
        # screens_widgets['kbd-layout'](),
        screens_widgets["sep"](),
        screens_widgets["layout"](),
    ]

    main_screen = Screen(top=bar.Bar(main_screen_widgets, **bar_commons))
    screens.append(main_screen)

    for _ in range(num_of_screens - 1):
        screens.append(Screen(top=bar.Bar(secondary_screen_widgets, **bar_commons)))


# This can make qtile crash for unknown reasons.
# See: https://github.com/qtile/qtile/issues/1346
# @hook.subscribe.screen_change
# def restart_on_randr(qtile, ev):
#     qtile.cmd_restart()


@hook.subscribe.client_new
def dialogs(window):
    try:
        cls = window.window.get_wm_class()[1]
    except:
        cls = ""

    should_float = (
        window.window.get_wm_type() == "dialog" or window.window.get_wm_transient_for()
    ) or (cls in ["Gnome-calculator", "Gcolor3", "Pinentry", "Gcr-prompter"])

    if should_float:
        window.floating = True


def main(qtile):
    setup_screens(qtile)
