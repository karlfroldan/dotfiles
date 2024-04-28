export const CalendarPopup = (monitor = 0) => {
    return Widget.Window({
        monitor,
        name: `calendarPopup${monitor}`,
        class_name: 'calendar-popup',
        anchor: ['top'],
        child: Widget.Label('Calendar Popup'),
    })
}
