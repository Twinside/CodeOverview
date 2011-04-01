#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gio
import sys

class OverViewImage:
    # when invoked (via signal delete_event), terminates the application.
    def close_application(self, widget, event, data=None):
        gtk.main_quit()
        return False

    # is invoked when the button is clicked.  It just prints a message.
    def button_clicked(self, widget, data=None):
        print "button %s clicked" % data

    def name( picture ):
        (width, height) = picture.get_image_size()
        realTop = self.beginning / height * actualHeight;
        realBottom = self.ending / height * actualHeight;
        
    def info_changed(self, monitor, fileObj, other_file = None, event_type = None, data = None):
        print("Detected Changes!")
        wakeFile = open(self.watchedFilename,"r")
        line = wakeFile.read()
        wakeFile.close()

        if line[-1] == '\n':
            line = line[0:-1]

        if line == "quit":
        	gtk.main_quit()
        	return

        [begin, end, imageFile] = line.split("?")
        self.beginning = int(begin)
        self.ending = int(begin)
        self.codeImage.set_from_file(imageFile)
        return

    def __init__(self, filename):
        self.watchedFilename = filename
        self.gFile = gio.File(filename)
        self.gFileMonitor = self.gFile.monitor_file()
        self.gFileMonitor.connect("changed", self.info_changed)

        # create the main window, and attach delete_event signal to terminating
        # the application
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.window.connect("delete_event", self.close_application)
        self.window.set_border_width(10)
        self.window.show()

        self.codeImage = gtk.Image()
        self.codeImage.show()

        # a button to contain the image widget
        #self.button = gtk.Button()
        #self.button.add(self.codeImage)
        #self.button.show()
        #self.window.add(self.button)
        #self.button.connect("clicked", self.button_clicked)
        self.window.add(self.codeImage)

if __name__ == "__main__":
    watchedFilename = "/tmp/overviewFile" + sys.argv[1] + '.txt'
    print(watchedFilename)
    OverViewImage(watchedFilename)
    gtk.main()

