using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Timers;

namespace WpfOverview
{
    /// 
    /// This filewatcher works around the 'issues'
    /// with Window's file watcher .NET FileSystemWatcher
    /// or Win32::ChangeNotify).  This code is written for
    /// WPF, but can be tweaked to work with Winforms as well.
    /// 
    /// It solves these two main problems:
    /// * The FileSystemWatcher calls multiple times on
    ///   a single change.
    /// * If my process changes the file, the
    ///   FileSystemWatcher calls me.
    /// 
    /// It solves the former by using a 100 ms timer to
    /// collapse multiple calls into a single call.  It
    /// solves the latter by storing the file size and
    /// time when this process writes a file, and
    /// comparing this to the values when notified by
    /// FileSystemWatcher.
    /// 
    /// Usage is straightforward, except that you must
    /// call CloseFileInThisProcess when you are closing
    /// the file that this watcher is watching.  It will
    /// carefully close the file in such a way that it
    /// can later tell if the change was by this process
    /// or another.
    /// 
    /// 
    public class FileWatcher : FileSystemWatcher
    {
        public delegate void FileChangedHandler(string filepath);

        public FileWatcher(string filepath, FileChangedHandler handler, FileChangedHandler deltedHandler) :
            base(System.IO.Path.GetDirectoryName(filepath)
                , System.IO.Path.GetFileName(filepath))
        {
            FilePath = filepath;
            Handler = handler;
            delHandler = deltedHandler;

            NotifyFilter =
                NotifyFilters.FileName |
                NotifyFilters.Attributes |
                NotifyFilters.LastAccess |
                NotifyFilters.LastWrite |
                NotifyFilters.Security |
                NotifyFilters.Size;

            Changed += new FileSystemEventHandler(delegate(object sender, FileSystemEventArgs e)
            {
                System.Windows.Application.Current.Dispatcher.BeginInvoke(
                    new VoidDelegate(this.FileChanged));
            });

            Deleted += new FileSystemEventHandler(delegate(object sender, FileSystemEventArgs e)
            {
                System.Windows.Application.Current.Dispatcher.BeginInvoke(
                    new VoidDelegate(this.FileDeleted));
            });

            UpdateFileInfo();
            Timer = new Timer(100);
            Timer.AutoReset = false;
            Timer.Elapsed += new ElapsedEventHandler(delegate(object sender, ElapsedEventArgs e)
            {
                System.Windows.Application.Current.Dispatcher.BeginInvoke(
                    new VoidDelegate(this.TimerElapsed));
            });
            EnableRaisingEvents = true;
        }

        /// 
        /// This only works with StreamWriters.  You may need
        /// to make your own version for whatever writer you are
        /// using.
        /// 
        /// How this works:  It stores the file size before
        /// calling close.  Then, after close it grabs the write
        /// time.  There is a minute corner case here:  If
        /// another process gets in and writes the file after
        /// the close, but before the GetLastWriteTime call, and the 
        /// file is the same length, we will not detect the change.  
        /// If that is critical, one could do a checksum on the file....
        /// 
        public void CloseFileInThisProcess(StreamWriter writer)
        {
            writer.Flush();
            LastFileLength = writer.BaseStream.Length;
            writer.Close();
            LastWriteTime = File.GetLastWriteTime(FilePath);
        }


        void UpdateFileInfo()
        {
            var fileInfo = new FileInfo(FilePath);
            LastWriteTime = fileInfo.LastWriteTime;
            LastFileLength = fileInfo.Length;
        }

        public delegate void VoidDelegate();

        void FileChanged()
        {
            var fileInfo = new FileInfo(FilePath);
            if (LastWriteTime != fileInfo.LastWriteTime || LastFileLength != fileInfo.Length)
                if (!Timer.Enabled)
                    Timer.Start();
        }

        void FileDeleted() { delHandler(FilePath); }

        void TimerElapsed()
        {
            UpdateFileInfo();
            Handler(FilePath);
        }

        string FilePath;
        FileChangedHandler delHandler;
        FileChangedHandler Handler;
        DateTime LastWriteTime;
        long LastFileLength;
        Timer Timer;
    }
}
