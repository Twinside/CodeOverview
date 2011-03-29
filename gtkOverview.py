#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gio

class OverViewImage:
    # when invoked (via signal delete_event), terminates the application.
    def close_application(self, widget, event, data=None):
        gtk.main_quit()
        return False

    # is invoked when the button is clicked.  It just prints a message.
    def button_clicked(self, widget, data=None):
        print "button %s clicked" % data

    def info_changed(self, monitor, fileObj, other_file = None, event_type = None, data = None):
        wakeFile = open(self.watchedFilename,"r")
        line = wakeFile.read()
        wakeFile.close()

        if line[-1] == '\n':
            line = line[0:-1]

        [begin, end, imageFile] = line.split("?")
        return

    def __init__(self, filename):
        self.watchedFilename = filename
        self.gFile = gio.File(filename)
        self.gFileMonitor = self.gFile.monitor_file()
        self.gFileMonitor.connect("changed", self.info_changed)

        # create the main window, and attach delete_event signal to terminating
        # the application
        window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        window.connect("delete_event", self.close_application)
        #window.set_border_width(10)
        window.show()

        image = gtk.Image()
        image.set_from_file("CodeOverview.png")
        image.show()
        # a button to contain the image widget
        button = gtk.Button()
        button.add(image)
        button.show()
        window.add(button)
        button.connect("clicked", self.button_clicked, "3")

def main():
    gtk.main()
    return 0

if __name__ == "__main__":
    OverViewImage("testf")
    main()

