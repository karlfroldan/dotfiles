const hyprland = await Service.import('hyprland')
const battery = await Service.import('battery')
const network = await Service.import('network')
import { CalendarPopup } from './calendar_popup.js'

const date = Variable('', {
    poll: [1000, 'date "+%H:%M, %b %e, %Y"'],
})

const Clock = () => {
    return Widget.Button({
        child: Widget.Label({
            label: date.bind(),
        }),
        onClicked: () => CalendarPopup(),
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
