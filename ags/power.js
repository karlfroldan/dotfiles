const ShutdownIcon = Widget.Icon({icon: 'system-shutdown-symbolic'})
const RebootIcon = Widget.Icon({icon: 'system-reboot-symbolic'})
const LogoutIcon = Widget.Icon({icon: 'system-log-out-symbolic'})

export const PowerButton = () => {
    return Widget.Button({
        child: ShutdownIcon,
    })
}
