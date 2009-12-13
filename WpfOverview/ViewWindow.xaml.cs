using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Windows.Threading;
using System.Runtime.InteropServices;
using System.Diagnostics;
using System.Windows.Interop;

namespace WpfOverview
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class ViewWindow : Window
    {
        FileWatcher watcher;

        int followedPid;
        Process followedProcess;
        DispatcherTimer windowFollowTimer;
        RECT oldPos;
        RECT followedWindowSize;
        int lastWindowMove;
        IntPtr thisHandle; 

        const int maxWaitDelay = 250;
        TimeSpan fastTimerInterval = TimeSpan.FromMilliseconds(10);
        TimeSpan windowFollowDelay = TimeSpan.FromMilliseconds(270);

        public ViewWindow()
        {
            InitializeComponent();
            watcher = null;

            windowFollowTimer = new DispatcherTimer();
            windowFollowTimer.Tick += new EventHandler(FollowPIDWindow);
            windowFollowTimer.Interval = windowFollowDelay;
            lastWindowMove = maxWaitDelay + 1;

            followedWindowSize = new RECT();
            oldPos = new RECT();
        }

        [DllImport("user32.dll")]
        static extern bool SetWindowPos(IntPtr hWnd, IntPtr hWndInsertAfter, int X, int Y, int cx, int cy, uint uFlags);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        static extern bool GetWindowRect(IntPtr hWnd, ref RECT lpRect);

        [StructLayout(LayoutKind.Sequential)]
        private struct RECT
        {
            public int Left;
            public int Top;
            public int Right;
            public int Bottom;
        }

        protected override void OnSourceInitialized(EventArgs e)
        {
            HwndSource hwndSource = PresentationSource.FromVisual(this) as HwndSource;

            if (hwndSource != null)
                thisHandle = hwndSource.Handle;

            string watchedFile =
                System.Environment.GetEnvironmentVariable("TEMP")
                + "\\overviewFile.txt";

            // create an empty file to sure it exists before anything
            using (var file = File.Create(watchedFile)) {}

            watcher = new FileWatcher(watchedFile
                                     , new FileWatcher.FileChangedHandler(onFileChange));
            FollowedPid = -1;
        }


        void FollowPIDWindow(object sender, EventArgs e)
        {
            try
            {
                followedProcess.Refresh();

                // get the followed window handle
                IntPtr windowHandle = followedProcess.MainWindowHandle;

                GetWindowRect(windowHandle, ref followedWindowSize);

                int difference = Math.Abs(oldPos.Bottom - followedWindowSize.Bottom)
                               + Math.Abs(oldPos.Top - followedWindowSize.Top)
                               + Math.Abs(oldPos.Left - followedWindowSize.Left)
                               + Math.Abs(oldPos.Right - followedWindowSize.Right);

                if ( difference > 0 )
                {
                    Left = Math.Max( followedWindowSize.Left - Width, 0 );
                    Top = followedWindowSize.Top;
                    Height = followedWindowSize.Bottom - followedWindowSize.Top;

                    oldPos = followedWindowSize;
                    // here we handle the speedup to follow the window
                    // to obtain a smoother move
                    lastWindowMove = 0;
                    if (windowFollowTimer.Interval != fastTimerInterval)
                        windowFollowTimer.Interval = fastTimerInterval;
                }
                else if (windowFollowTimer.Interval.Milliseconds == fastTimerInterval.Milliseconds)
                {
                    // when the window don't move anymore, we hope to bring
                    // back the old slow timer.
                    lastWindowMove += fastTimerInterval.Milliseconds;
                    if (lastWindowMove > maxWaitDelay)
                        windowFollowTimer.Interval = windowFollowDelay;
                }

                GetWindowRect(thisHandle, ref followedWindowSize);
                SetWindowPos( thisHandle
                            , windowHandle
                            , followedWindowSize.Left
                            , followedWindowSize.Top
                            , followedWindowSize.Right - followedWindowSize.Left
                            , followedWindowSize.Bottom - followedWindowSize.Top
                            , 0);
            }
            catch (PlatformNotSupportedException)
            {   /* I'm not sure about this one, but I'm
                 * sure we can't track it's window */
                FollowedPid = -1;
            }
            catch (NotSupportedException)
            {   /* Process may have no window */
                FollowedPid = -1;
            }
            catch (InvalidOperationException)
            {   /* Process may be terminated */
                FollowedPid = -1;
            }
        }

        private int FollowedPid
        {
            get { return FollowedPid; }
            set
            {
                followedPid = value;

                if (followedPid >= 0)
                {
                    followedProcess = Process.GetProcessById(followedPid);
                    FollowPIDWindow(null, null);
                    if (!windowFollowTimer.IsEnabled)
                        windowFollowTimer.Start();

                    Visibility = Visibility.Visible;
                }
                else
                {
                    Visibility = Visibility.Hidden;
                    if (windowFollowTimer.IsEnabled)
                        windowFollowTimer.Stop();
                }
            }
        }

        private void onFileChange(string filename)
        {
            using (FileStream fs = File.OpenRead(filename))
            using (TextReader reader = new StreamReader(fs))
            {
                try
                {
                    string[]    info = reader.ReadLine().Split('|');
                    int newPid = int.Parse( info[0] );

                    BitmapImage newOverview = new BitmapImage();
                    newOverview.BeginInit();
                    newOverview.CreateOptions = BitmapCreateOptions.IgnoreImageCache;
                    newOverview.CacheOption = BitmapCacheOption.OnLoad;
                    newOverview.UriSource = new Uri(info[1]);
                    newOverview.EndInit();

                    //Width = Math.Max( newOverview.Width, 100 );
                    //pictureViewer.Height = newOverview.Height;
                    pictureViewer.Source = newOverview;

                    //pictureViewer.UpdateLayout();
                    //pictureViewer.InvalidateVisual();

                    // if all went well we follow a new process
                    FollowedPid = newPid;
                }
                catch (UriFormatException)
                {
                    /* Bad image... putting empty image instead */
                    pictureViewer.Source = null;
                }
                // Handling int parsing
                catch (OverflowException)
                { /* PID > int32, ok error no problem */ FollowedPid = -1; }
                catch (FormatException)
                { /* We can safely ignore it */ FollowedPid = -1; }
                catch (IOException)
                { /* can happen, not a problem in this case. */ }
                catch (ArgumentOutOfRangeException)
                { /* we're searching for a PID and a path, nothing big
                   * Ignore if to big */
                    FollowedPid = -1;
                }
            }
        }
    }
}
