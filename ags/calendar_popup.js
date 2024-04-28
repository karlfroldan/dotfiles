export const CalendarPopup = (name, monitor = 0) => {

    const Calendar = Widget.Calendar()
    
    return Widget.Window({
        monitor,
        name,
        class_name: 'calendar-popup',
        margins: [35, 0],
        anchor: ['top'],
        child: Calendar,
    })
}
