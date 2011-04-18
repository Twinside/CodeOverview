#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gio
import sys
import cairo
import os
from ctypes import *

class OverViewImage:
    # when invoked (via signal delete_event), terminates the application.
    def close_application(self, widget, event, data=None):
        gtk.main_quit()
        return False

    # is invoked when the button is clicked.  It just prints a message.
    def image_clicked(self, widget, data=None):
        if not self.initiated:
            return
        yPos = int(data.y)
        #textVersion = ''.join(map(lambda n: '\\[XK_' + n + ']', str(yPos)))
        textVersion = str(int(yPos * self.imageHeight / self.actualHeight))
        command = 'xvkbd -text "' + textVersion + 'ggzz" -window ' + str(self.windowId)

        relativeHeight = (self.realBottom - self.realTop) / 2
        self.realTop = max(0, yPos - relativeHeight)
        self.realBottom = yPos + relativeHeight
        os.system(command)
        self.drawArea.queue_draw()

    def updateViewSizeInformation(self, picture):
        (width, height) = (picture.get_width(), picture.get_height())
        self.realTop = float(self.beginning) / height * self.actualHeight;
        self.realBottom = float(self.ending) / height * self.actualHeight;
        self.imageHeight = height
        
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
        wakeFile = open(self.watchedFilename,"r")
        line = wakeFile.read()
        wakeFile.close()

        if line[-1] == '\n':
            line = line[0:-1]

        if line == "quit":
        	gtk.main_quit()
        	return

        [begin, end, backColor, viewRectColor, winId, winX, winY, imageFile] = line.split("?")
        self.beginning = int(begin)
        self.ending = int(end)
        self.updateImage(imageFile)
        self.windowId = int(winId)
        self.backColor = gtk.gdk.color_parse(backColor)
        self.window.modify_bg(gtk.STATE_NORMAL, self.backColor)
        
        (winWidth, winHeight) = self.window.get_size()
        self.window.move(int(winX) - winWidth, int(winY))

        self.rectColor = gtk.gdk.color_parse(viewRectColor)
        self.initiated = True

    def area_draw(self, area, event):
        if not self.initiated:
        	return

        (width, wholeHeight) = self.drawArea.window.get_size()
        height = self.realBottom - self.realTop

        cr = self.drawArea.window.cairo_create()

        cr.set_source_rgba(self.backColor.red_float, 
                           self.backColor.green_float, 
                           self.backColor.blue_float, 1.0)
        cr.rectangle(0, 0, width, wholeHeight)
        cr.fill()

        cr.set_source_rgba(1.0, 1.0, 1.0, 1.0)
        cr.set_source_pixbuf(self.scaledPixbuf, 0, 0)
        cr.paint()

        cr.set_source_rgba(self.rectColor.red_float, 
                           self.rectColor.green_float, 
                           self.rectColor.blue_float, 0.3)
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
        self.window.resize(80, 500)
        self.window.move(0, 0)
        self.window.set_title(title)
        self.window.set_icon(None)
        self.window.show()

        self.backColor = gtk.gdk.color_parse('#FFFFFF')

        self.drawArea = gtk.DrawingArea()
        self.drawArea.set_events(gtk.gdk.BUTTON_PRESS_MASK | gtk.gdk.BUTTON_RELEASE_MASK)
        self.drawArea.connect("expose-event", self.area_draw )
        self.drawArea.connect("button-release-event", self.image_clicked )
        self.drawArea.show()

        self.window.add(self.drawArea)

if __name__ == "__main__":
    watchedFilename = "/tmp/overviewFile" + sys.argv[1] + '.txt'
    OverViewImage(sys.argv[1], watchedFilename)
    gtk.main()

