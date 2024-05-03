const hyprland = await Service.import('hyprland')
const battery = await Service.import('battery')
const network = await Service.import('network')
const audio = await Service.import('audio')
import { CalendarPopup } from './calendar_popup.js'
import { PowerButton } from './power.js'

const date = Variable('', {
    poll: [1000, 'date "+%H:%M, %b %e, %Y"'],
})

const Clock = (monitor = 0) => {
    const calendar_name = `calendarPopup${monitor}`
    const calendar_popup = CalendarPopup(calendar_name)

    App.addWindow(calendar_popup);
    // Window is initially closed
    App.closeWindow(calendar_name);
    
    return Widget.Button({
        child: Widget.Label({
            label: date.bind(),
        }),
        onClicked: () => App.toggleWindow(calendar_name),
    })
}


/// The RevealerButton will have a button and an icon that may change given the
/// status of the sliding bar.
const RevealerButton = (
    slider_fn, // function that will execute onChange of slider
    slider_value, // value of the slider.
    icon = 'emblem-default-symbolic',
    icon_hook = () => {},
) => {
    const is_revealed = Variable(false)

    const button_icon = Widget.Icon({icon})
    // Clicking the button will disable the reveal of the slider.
    const button = Widget.Button({
        child: button_icon,
        onHover: () => {
            is_revealed.value = true
        },
        onClicked: () => {
            is_revealed.value = !is_revealed.value
        },
    })

    // This is the child of the revealer
    const slider = Widget.Slider({
        vertical: false,
        draw_value: false,
        hexpand: true,
        value: slider_value,
        min: 0,
        max: 1,
        onChange: slider_fn,
    })

    const revealer = Widget.Revealer({
        revealChild: is_revealed.bind(),
        transitionDuration: 1000,
        transition: 'slide_right',
        child: slider,
    })

    return Widget.Box({
        spacing: 2,
        vertical: false,
        css: 'min-width: 180px',
        children: [
            button,
            revealer,
        ],
    })
}

/// My laptop currently only has Wi-Fi so let's only consider that for now.
const NetworkCenter = () => {
    return Widget.Box({
        spacing: 8,
        vertical: false,
        children: [
            Widget.Label({
                label: Utils.merge([
                    network.wifi.bind('internet'), network.wifi.bind('ssid'),
                ], (wireless_status, ssid) => {
                    if (wireless_status != 'disconnected')
                        return ssid
                    else
                        return 'Not connected'
                }),
            }),
            Widget.Icon({
                icon: network.wifi.bind('icon-name')
            }),
        ],
    })
}

/// For the battery progress, I prefer that it shows the battery status
/// alongside the actual percentage so let's use a Box with a horizontal orientation.
const BatteryProgress = () => {
    return Widget.Box({
        spacing: 1,
        vertical: false,
        children: [
            Widget.Label({
                label: battery.bind('percent').as(p => String(p) + '%'),
            }),
            Widget.Icon({
                icon: battery.bind('icon-name'),
            }),
        ]
    })
}

const AudioBar = () => {
    return RevealerButton(
        ({value}) => {
            audio['speaker'].volume = value
        },
        audio['speaker'].bind('volume'),
        'audio-speakers-symbolic'
    )
}

const Left = () => {
    return Widget.Label('Left Widget')
}

const Center = () => {
    return Widget.Box({
        hpack: 'center',
        spacing: 64,
        vertical: false,
        children: [
            Clock(),
        ],
    })
}

const Right = () => {
    return Widget.Box({
        hpack: 'center',
        spacing: 64, 
        homogeneous: false,
        vertical: false,
        children: [
            BatteryProgress(),
            NetworkCenter(),
            AudioBar(),
            PowerButton(),
        ],
    })
}

const Bar = (monitor = 0) => {
    return Widget.Window({
        monitor,
        name: `bar${monitor}`,
        layer: 'top',
        anchor: ['top', 'left', 'right'],
        child: Widget.CenterBox({
            vertical: false,
            startWidget: Left(),
            centerWidget: Center(),
            endWidget: Right(),
        }),
        // date +'%a, %k:%M %m/%H/%Y'
    });
};

App.config({
    style: './style.css',
    windows: [
        // This is where the window definitions will go.
        Bar()
    ]
});
