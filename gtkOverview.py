#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gio
import sys
import cairo
from ctypes import *

class XScreen(Structure):
    _fields_ = [
        ("ext_data", c_void_p),
        ("display", c_void_p),
        ("root", c_int),
        ("width", c_int),
        ("height", c_int),
        ("mwidth", c_int),
        ("mheight", c_int),
        ("ndepths", c_int),
        ("depths", c_void_p),
        ("root_depth", c_int),
        ("root_visual", c_void_p),
        ("default_gc", c_void_p),
        ("cmap", c_int),
        ("white_pixel", c_long),
        ("black_pixel", c_long),
        ("max_maps", c_int),
        ("min_maps", c_int),
        ("backing_store", c_int),
        ("save_unders", c_bool),
        ("root_input_mask", c_long)]

class XDisplay(Structure):
    _fields_ = [
        ("ext_data", c_void_p),
        ("private1", c_void_p),
        ("fd", c_int),
        ("private2", c_int),
        ("proto_major_version", c_int),
        ("proto_minor_version", c_int),
        ("vendor", c_void_p),
        ("private3", c_int),
        ("private4", c_int),
        ("private5", c_int),
        ("private6", c_int),
        ("resource_alloc", c_void_p),
        ("byte_order", c_int),
        ("bitmap_unit", c_int),
        ("bitmap_pad", c_int),
        ("bitmap_bit_order", c_int),
        ("nformats", c_int),
        ("pixmap_format", c_void_p),
        ("private8", c_int),
        ("release", c_int),
        ("private9", c_void_p),
        ("private10", c_void_p),
        ("qlen", c_int),
        ("last_request_read", c_long),
        ("request", c_long),
        ("private11", c_void_p),
        ("private12", c_void_p),
        ("private13", c_void_p),
        ("private14", c_void_p),
        ("max_request_size", c_int),
        ("db", c_void_p),
        ("private15", c_void_p),
        ("display_name", c_void_p),
        ("default_screen", c_int),
        ("nscreens", c_int),
        ("screens", POINTER(XScreen)),
        ("motion_buffer", c_long),
        ("private16", c_long),
        ("min_keycode", c_int),
        ("max_keycode", c_int),
        ("private17", c_void_p),
        ("private18", c_void_p),
        ("private19", c_int),
        ("xdefaults", c_void_p)]

class XEvent(Structure):
    _fields_ = [("keytype", c_int),
                ("serial", c_ulong),
                ("send_event", c_bool),
                ("display", POINTER(XDisplay)),
                ("window", c_int),
                ("root", c_int),
                ("subwindow", c_int),
                ("time", c_int),
                ("x", c_int),
                ("y", c_int),
                ("x_root", c_int),
                ("y_root", c_int),
                ("state", c_int),
                ("keycode", c_int),
                ("same_screen", c_bool)]

class XKeySender:
    def __init__(self, windowId):
        self.windowId = windowId
        self.lib = CDLL("libX11.so.6")

        self.key_g = 0x67
        self.key_z = 0x7A

        # Keys for all the numbers
        self.key_num = range(0x30, 0x40)

    def openDisplay(self):
        xOpenDisplay = self.lib.XOpenDisplay
        xOpenDisplay.restype = POINTER(XDisplay)
        return xOpenDisplay(c_char_p(0))

    def rootWindow(self, display):
        defaultScreen = display.contents.default_screen
        screen = display.contents.screens[defaultScreen]
        return screen.root
        
    def sendChar(self, display, keySim):
        keyPressMask = 1
        keyPress = 2
        keyRelease = 3
        none = 0
        currentTime = 0
        keyCode = self.lib.XKeysymToKeycode(display, c_int(keySim))
        event = XEvent( keyPress # type
                      , 0 # serial
                      , 0 # send_event
                      , display # display
                      , self.windowId # window
                      , self.rootWindow(display) # rootwindow
                      , none #subwindow
                      , currentTime # time
                      , 1 # x
                      , 1 # y
                      , 1 #xroot
                      , 1 #yroot
                      , 0 #state
                      , keyCode #keycode
                      , 1 #same_screen )
                      )

        ret = self.lib.XSendEvent(display, 
                            c_int(self.windowId),
                            c_bool(True),
                            c_int(keyPressMask),
                            byref(event))

        print(ret)
        event.eventType = keyRelease

        ret = self.lib.XSendEvent(display, 
                            c_int(self.windowId),
                            c_bool(True),
                            c_int(keyPressMask),
                            byref(event))
        print(ret)
        self.lib.XSync(display, 1)

    def sendLineChange(self, line):
        display = self.openDisplay()

        for c in str(line):
            self.sendChar(display, self.key_num[int(c)])

        self.sendChar(display, self.key_g)
        self.sendChar(display, self.key_g)

        self.sendChar(display, self.key_z)
        self.sendChar(display, self.key_z)

        self.lib.XCloseDisplay(display)

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
        #self.keySender.sendLineChange(yPos)

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
        self.keySender.windowId = int(winId)

        self.backColor = gtk.gdk.color_parse(backColor)
        self.window.modify_bg(gtk.STATE_NORMAL, self.backColor)

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
        #self.drawArea.connect("button-press-event", self.image_clicked )
        self.drawArea.show()

        self.window.add(self.drawArea)

        self.keySender = XKeySender(0)

if __name__ == "__main__":
    watchedFilename = "/tmp/overviewFile" + sys.argv[1] + '.txt'
    OverViewImage(sys.argv[1], watchedFilename)
    gtk.main()

