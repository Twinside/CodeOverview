#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gio
import sys
import cairo

class OverViewImage:
    # when invoked (via signal delete_event), terminates the application.
    def close_application(self, widget, event, data=None):
        gtk.main_quit()
        return False

    # is invoked when the button is clicked.  It just prints a message.
    def button_clicked(self, widget, data=None):
        print "button %s clicked" % data

    def updateViewSizeInformation(self, picture):
        (width, height) = (picture.get_width(), picture.get_height())
        self.realTop = float(self.beginning) / height * self.actualHeight;
        self.realBottom = float(self.ending) / height * self.actualHeight;
        
    def updateImage(self, newFilename):
        pixbuf = gtk.gdk.pixbuf_new_from_file(newFilename)

        imageWidth = pixbuf.get_width()
        imageHeight = pixbuf.get_height()

        (width, height) = self.window.get_size()

        width = min( imageWidth, width )
        height = min( imageHeight, height )

        self.actualHeight = height
        self.updateViewSizeInformation(pixbuf)

        self.codePixbuf = pixbuf
        self.scaledPixbuf = pixbuf.scale_simple( width, height, gtk.gdk.INTERP_BILINEAR)
        self.drawArea.queue_draw()

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

        [begin, end, backColor, imageFile] = line.split("?")
        self.beginning = int(begin)
        self.ending = int(end)
        self.updateImage(imageFile)

        color = gtk.gdk.color_parse(backColor)
        self.window.modify_bg(gtk.STATE_NORMAL, color)

        self.initiated = True

    def area_draw(self, area, event):
        if not self.initiated:
        	return

        cr = self.drawArea.window.cairo_create()
        cr.set_source_pixbuf(self.scaledPixbuf, 0, 0)
        cr.paint()

        (width, wholeHeight) = self.drawArea.window.get_size()
        height = self.realBottom - self.realTop

        cr.set_source_rgba(0.7, 0.7, 1.0, 0.6)
        cr.rectangle(0, int(self.realTop), width, int(height))
        cr.fill()
        

    def __init__(self, title, filename):
        self.initiated = False

        self.watchedFilename = filename
        self.gFile = gio.File(filename)
        self.gFileMonitor = self.gFile.monitor_file()
        self.gFileMonitor.connect("changed", self.info_changed)

        # create the main window, and attach delete_event signal to terminating
        # the application
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.window.connect("delete_event", self.close_application)
        self.window.set_border_width(0)
        self.window.resize(150, 500)
        self.window.set_title(title)
        self.window.show()

        self.drawArea = gtk.DrawingArea()
        self.drawArea.connect("expose-event", self.area_draw )
        self.drawArea.show()

        self.window.add(self.drawArea)

if __name__ == "__main__":
    watchedFilename = "/tmp/overviewFile" + sys.argv[1] + '.txt'
    print(watchedFilename)
    OverViewImage(sys.argv[1], watchedFilename)
    gtk.main()

