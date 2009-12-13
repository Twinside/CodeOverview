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
        IntPtr windowHandle;

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

        const UInt32 WM_KEYDOWN = 0x0100;
        const UInt32 WM_KEYUP = 0x0101;

        [DllImport("user32.dll")]
        static extern bool SetWindowPos(IntPtr hWnd, IntPtr hWndInsertAfter, int X, int Y, int cx, int cy, uint uFlags);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        static extern bool GetWindowRect(IntPtr hWnd, ref RECT lpRect);

        [return: MarshalAs(UnmanagedType.Bool)]
        [DllImport("user32.dll", SetLastError = true)]
        static extern bool PostMessage(IntPtr hWnd, UInt32 Msg, Int32 wParam, Int32 lParam);

        [StructLayout(LayoutKind.Sequential)]
        private struct RECT
        {
            public int Left;
            public int Top;
            public int Right;
            public int Bottom;
        }

        private void SendKeyToFollowedProcess( Key  k )
        {
            int vk = KeyInterop.VirtualKeyFromKey(k);
            // Ugly in the general case, but vim is built on windows
            // to handle this kind of things. As it's so simpler than
            // other techniques, use it.
            PostMessage( windowHandle, WM_KEYDOWN, vk, 0 );
            PostMessage( windowHandle, WM_KEYUP, vk, 0 );
        }

        protected override void OnSourceInitialized(EventArgs e)
        {
            HwndSource hwndSource = PresentationSource.FromVisual(this) as HwndSource;

            if (hwndSource != null)
                thisHandle = hwndSource.Handle;

            try
            {
                string[] args = System.Environment.GetCommandLineArgs();
                FollowedPid = int.Parse( args[args.Length - 1] );
            }
            // Handling int parsing
            // if we can't find a valid PID, we must shut down, there's nothing
            // we can do.
            catch (OverflowException) { Application.Current.Shutdown(); }
            catch (FormatException)   { Application.Current.Shutdown(); }

            string watchedFile =
                System.Environment.GetEnvironmentVariable("TEMP")
                + "\\overviewFile" + FollowedPid.ToString() + ".txt";

            // create an empty file to sure it exists before anything
            using (var file = File.Create(watchedFile)) {}

            watcher = new FileWatcher(watchedFile
                                     , new FileWatcher.FileChangedHandler(onFileChange));
        }


        void FollowPIDWindow(object sender, EventArgs e)
        {
            try
            {
                followedProcess.Refresh();

                // get the followed window handle
                windowHandle = followedProcess.MainWindowHandle;

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
                Application.Current.Shutdown();
            }
            catch (NotSupportedException)
            {   /* Process may have no window */
                Application.Current.Shutdown();
            }
            catch (InvalidOperationException)
            {   /* Process may be terminated */
                Application.Current.Shutdown();
            }
        }

        private int FollowedPid
        {
            get { return followedPid; }
            set
            {
                followedPid = value;

                followedProcess = Process.GetProcessById(followedPid);
                FollowPIDWindow(null, null);
                if (!windowFollowTimer.IsEnabled)
                    windowFollowTimer.Start();
            }
        }

        private void onFileChange(string filename)
        {
            using (FileStream fs = File.OpenRead(filename))
            using (TextReader reader = new StreamReader(fs))
            {
                try
                {
                    BitmapImage newOverview = new BitmapImage();
                    newOverview.BeginInit();
                    newOverview.CreateOptions = BitmapCreateOptions.IgnoreImageCache;
                    newOverview.CacheOption = BitmapCacheOption.OnLoad;
                    newOverview.UriSource = new Uri(reader.ReadLine());
                    newOverview.EndInit();

                    pictureViewer.Source = newOverview;
                }
                catch (UriFormatException)
                {
                    /* Bad image... putting empty image instead */
                    pictureViewer.Source = null;
                }
                catch (IOException)
                { /* can happen, not a problem in this case. */ }
                catch (ArgumentOutOfRangeException)
                { /* we're searching for a PID and a path, nothing big
                   * Ignore if to big */
                    Application.Current.Shutdown();
                }
            }
        }

        Key[] numbers = { Key.NumPad0, Key.NumPad1, Key.NumPad2
                        , Key.NumPad3, Key.NumPad4, Key.NumPad5
                        , Key.NumPad6, Key.NumPad6, Key.NumPad7
                        , Key.NumPad8, Key.NumPad9
                        };
        private void updateVimView( Point pos )
        {
            string  heightI = ((int)pos.Y + 1).ToString();

            //SendKeyToFollowedProcess(Key.Escape);
            //SendKeyToFollowedProcess(Key.I);
            foreach (char c in heightI)
                SendKeyToFollowedProcess( numbers[(int)c - (int)'0'] );

            SendKeyToFollowedProcess(Key.G);
            SendKeyToFollowedProcess(Key.G);
        }

        private void pictureViewer_MouseLeftButtonUp(object sender, MouseButtonEventArgs e)
            { updateVimView(e.GetPosition((Image)sender)); }

        private void pictureViewer_MouseMove(object sender, MouseEventArgs e)
        {
            if (e.LeftButton == MouseButtonState.Pressed)
                updateVimView(e.GetPosition((Image)sender));
        }
    }
}
