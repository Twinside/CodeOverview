using System;
using System.Collections.Generic;
using System.IO;
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
using System.ComponentModel;

namespace WpfOverview
{
    /// <summary>
    /// Meh!
    /// </summary>
    public partial class ViewWindow : Window, INotifyPropertyChanged
    {
        /// <summary>
        /// Used to track a file in $TEMP
        /// </summary>
        FileWatcher watcher;

        /// <summary>
        /// PID of the followed vim Instance
        /// </summary>
        int followedPid;

        /// <summary>
        /// We grab some information from the process regularly,
        /// so we keep it here.
        /// </summary>
        Process followedProcess;

        /// <summary>
        /// Handle of this wpf window. Usefull
        /// to make dirty tricks at the Win32 level.
        /// </summary>
        IntPtr thisHandle;

        /// <summary>
        /// Size of window pointed by thisHandle.
        /// </summary>
        RECT thisWindowSize;

        /// <summary>
        /// Main window of the followed process.
        /// Used to displace & resize it.
        /// </summary>
        IntPtr windowHandle;

        /// <summary>
        /// Previous size of windowHandle.
        /// Used to detect position and size change.
        /// </summary>
        RECT oldPos;

        /// <summary>
        /// Current position of windowHandle
        /// </summary>
        RECT followedWindowSize;

        WINDOWPLACEMENT windowInformation;

        /// <summary>
        /// Time of wait between the last move/resize
        /// and the restoration of the 'slow' timer.
        /// </summary>
        const int maxWaitDelay = 250;

        /// <summary>
        /// Speed of the 'quick' timer to follow the window of
        /// other process while on move.
        /// </summary>
        TimeSpan fastTimerInterval = TimeSpan.FromMilliseconds(10);

        /// <summary>
        /// Speed of the slow timer to follow the window of the
        /// other process.
        /// </summary>
        TimeSpan windowFollowDelay = TimeSpan.FromMilliseconds(270);

        /// <summary>
        /// Timer using fastTimerInterval & windowFollowDelay.
        /// </summary>
        DispatcherTimer windowFollowTimer;

        /// <summary>
        /// Track the time elapsed since the last followed window
        /// move/resize.
        /// </summary>
        int lastWindowMove;

        public ViewWindow()
        {
            InitializeComponent();
            watcher = null;

            windowFollowTimer = new DispatcherTimer();
            windowFollowTimer.Tick += new EventHandler(FollowPIDWindow);
            windowFollowTimer.Interval = windowFollowDelay;

            // just to be sure to start with a long timer.
            lastWindowMove = maxWaitDelay + 1;

            followedWindowSize = new RECT();
            oldPos = new RECT();

            windowInformation = new WINDOWPLACEMENT();
            windowInformation.length =     3 * sizeof(int)
                                     + 2 * 2 * sizeof(int)  // POINT
                                     + 1 * 4 * sizeof(int); // RECT
            #if LOGGING
            logEvent( "= Init : " + Environment.OSVersion.Version.Major.ToString() );
            #endif
        }

        #region Win32 pInvoke
        const UInt32 WM_KEYDOWN = 0x0100;
        const UInt32 WM_KEYUP = 0x0101;
        const UInt32 SW_HIDE =             0;
        const UInt32 SW_SHOWNORMAL =       1;
        const UInt32 SW_NORMAL =           1;
        const UInt32 SW_SHOWMINIMIZED =    2;
        const UInt32 SW_SHOWMAXIMIZED =    3;
        const UInt32 SW_MAXIMIZE =         3;
        const UInt32 SW_SHOWNOACTIVATE =   4;
        const UInt32 SW_SHOW =             5;
        const UInt32 SW_MINIMIZE =         6;
        const UInt32 SW_SHOWMINNOACTIVE =  7;
        const UInt32 SW_SHOWNA =           8;
        const UInt32 SW_RESTORE =          9;
        const UInt32 SW_SHOWDEFAULT =      10;
        const UInt32 SW_FORCEMINIMIZE =    11;
        const UInt32 SW_MAX =              11;

        [DllImport("user32.dll")]
        static extern bool SetWindowPos(IntPtr hWnd, IntPtr hWndInsertAfter, int X, int Y, int cx, int cy, uint uFlags);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        static extern bool GetWindowRect(IntPtr hWnd, ref RECT lpRect);

        [return: MarshalAs(UnmanagedType.Bool)]
        [DllImport("user32.dll", SetLastError = true)]
        static extern bool PostMessage(IntPtr hWnd, UInt32 Msg, Int32 wParam, Int32 lParam);

        [DllImport("user32.dll")]
        [return: MarshalAs(UnmanagedType.Bool)]
        static extern bool GetWindowPlacement(IntPtr hWnd, ref WINDOWPLACEMENT lpwndpl);

        [DllImport("user32.dll")]
        static extern bool SetWindowPlacement(IntPtr hWnd, [In] ref WINDOWPLACEMENT lpwndpl);

        [DllImport("user32.dll")]
        private static extern int ShowWindow(IntPtr hWnd, UInt32 nCmdShow);

        [StructLayout(LayoutKind.Sequential)]
        private struct WINDOWPLACEMENT
        {
            public UInt32 length;
            public UInt32 flags;
            public UInt32 showCmd;
            public POINT ptMinPosition;
            public POINT ptMaxPosition;
            public RECT rcNormalPosition;
        }

        [StructLayout(LayoutKind.Sequential)]
        private struct POINT
        {
            public int x;
            public int y;
        }

        [StructLayout(LayoutKind.Sequential)]
        private struct RECT
        {
            public int Left;
            public int Top;
            public int Right;
            public int Bottom;
            public static int Diff( RECT a, RECT b)
            {
                return Math.Abs(a.Top - b.Top) + Math.Abs(a.Bottom - b.Bottom)
                     + Math.Abs(a.Left - a.Left) + Math.Abs(a.Right - b.Right);

            }
        }
        #endregion

        /// <summary>
        /// Send a key to the followed window.
        /// The key is sent with 2 windows messages : a
        /// WM_KEYDOWN followed by a WM_KEYUP.
        /// </summary>
        /// <param name="k">key to send</param>
        private void SendKeyToFollowedProcess( Key  k )
        {
            int vk = KeyInterop.VirtualKeyFromKey(k);
            // Ugly in the general case, but vim is built on windows
            // to handle this kind of things. As it's so simpler than
            // other techniques, use it.
            PostMessage( windowHandle, WM_KEYDOWN, vk, 0 );
            PostMessage( windowHandle, WM_KEYUP, vk, 0 );
        }

        /// <summary>
        /// Perform some initializations which can only be done once
        /// the window is fully loaded.
        /// </summary>
        /// <param name="e"></param>
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
                                     , new FileWatcher.FileChangedHandler(onFileChange)
                                     , new FileWatcher.FileChangedHandler(a => Application.Current.Shutdown())
                                     );
        }

        enum TrackingState
        {
            TrackingMove,
            WaitingMaximization,
            Maximized
        }

        TrackingState currentState;

        /// <summary>
        /// In this method, we wait till the window has finished it's animation
        /// of maximization to tackle ourselves nearby, without changing the
        /// maximization state.
        /// </summary>
        void    waitingMaximization()
        {
            GetWindowRect(windowHandle, ref followedWindowSize);

            // animation is finished.
            if ( RECT.Diff( oldPos, followedWindowSize ) == 0 )
            {
                // ok, the user just to maximize it, we're gonna give it to him
                GetWindowRect( thisHandle, ref thisWindowSize);

                int thisWidth = thisWindowSize.Right - thisWindowSize.Left;

                Left = Math.Max( followedWindowSize.Left - thisWidth, 0 );
                Top = followedWindowSize.Top;
                Height = followedWindowSize.Bottom - followedWindowSize.Top;

                SetWindowPos( windowHandle
                            , IntPtr.Zero
                            , followedWindowSize.Left + thisWidth
                            , followedWindowSize.Top
                            , followedWindowSize.Right - followedWindowSize.Left - thisWidth
                            , followedWindowSize.Bottom - followedWindowSize.Top
                            , 0
                            );

                SetWindowPos( thisHandle
                            , windowHandle
                            , followedWindowSize.Left
                            , followedWindowSize.Top
                            , thisWidth
                            , followedWindowSize.Bottom - followedWindowSize.Top
                            , 0
                            );

                currentState = TrackingState.Maximized;
                windowFollowTimer.Interval = windowFollowDelay;
            }

            oldPos = followedWindowSize;
        }

        void    maximized()
        {
            GetWindowPlacement( windowHandle, ref windowInformation);

            if ( (windowInformation.flags & SW_SHOWMAXIMIZED) == 0
              && (windowInformation.flags & SW_MAXIMIZE) == 0)
            {
                currentState = TrackingState.TrackingMove;
            }
        }

        void    tracking()
        {
            GetWindowRect(windowHandle, ref followedWindowSize);
            GetWindowPlacement( windowHandle, ref windowInformation);

            // if the window has been maximized
            if ( (windowInformation.flags & SW_SHOWMAXIMIZED) != 0
              || (windowInformation.flags & SW_MAXIMIZE) != 0)
            {
                currentState = TrackingState.WaitingMaximization;
                windowFollowTimer.Interval = fastTimerInterval;
            }
            else
            {
                int difference = Math.Abs(oldPos.Bottom - followedWindowSize.Bottom)
                               + Math.Abs(oldPos.Top - followedWindowSize.Top)
                               + Math.Abs(oldPos.Left - followedWindowSize.Left)
                               + Math.Abs(oldPos.Right - followedWindowSize.Right);

                if ( difference > 0 )
                {
                    #if LOGGING
                    logEvent( "% Moved < " + followedWindowSize.Left.ToString()
                                    + ", " + followedWindowSize.Top.ToString()
                                    + ", " + (followedWindowSize.Right - followedWindowSize.Left).ToString()
                                    + ", " + (followedWindowSize.Bottom - followedWindowSize.Top).ToString()
                                    + " >" );
                    #endif

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
        }

        /// <summary>
        /// Function called by the windowFollowTimer to adjust this
        /// window to the size of the tracked Window.
        /// </summary>
        /// <param name="sender">unused</param>
        /// <param name="e">unused</param>
        void FollowPIDWindow(object sender, EventArgs e)
        {
            followedProcess.Refresh();
            // get the followed window handle
            windowHandle = followedProcess.MainWindowHandle;

            try
            {
                switch (currentState)
                {
                    case TrackingState.TrackingMove: tracking(); break;
                    case TrackingState.WaitingMaximization: waitingMaximization(); break;
                    case TrackingState.Maximized: maximized(); break;
                }
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

        /// <summary>
        /// Proprety to set followed PID, start
        /// timer if PID is valid.
        /// </summary>
        private int FollowedPid
        {
            get { return followedPid; }
            set
            {
                followedPid = value;

                try
                {
                    followedProcess = Process.GetProcessById(followedPid);
                    FollowPIDWindow(null, null);

                    if (!windowFollowTimer.IsEnabled)
                        windowFollowTimer.Start();
                }
                catch (ArgumentException)
                {   /* We didn't found a process with
                     * the good PID, we must shut down */
                    Application.Current.Shutdown();
                }
            }
        }

        #region WPF Binding
        /// <summary>
        /// Height of the blue rectangle helping to visualize
        /// the current vim's window view.
        /// </summary>
        double viewRectHeight = 0.0;

        /// <summary>
        /// Complete size of the blue rectangle helping
        /// to visualize the current vim's window view.
        /// </summary>
        Thickness viewRectMargin;

        /// <summary>
        /// Provide a WPF Binding as ViewRectMargin for
        /// the size of Vim's window View.
        /// </summary>
        public Thickness ViewRectMargin
        {
            get { return viewRectMargin; }
            set {
                viewRectMargin = value;
                OnPropertyChanged("ViewRectMargin");
            }
        }

        /// <summary>
        /// Provide a WPF Binding as ViewRectTop for
        /// the top of Vim's window view.
        /// </summary>
        public double ViewRectTop
        {
            get { return viewRectMargin.Top; }
            set {
                viewRectMargin.Top = value;
                OnPropertyChanged("ViewRectTop");
                OnPropertyChanged("ViewRectMargin");
            }
        }

        /// <summary>
        /// Provide a WPF Binding as ViewRectHeight for
        /// the height of Vim's window view.
        /// </summary>
        public double ViewRectHeight
        {
            get { return viewRectHeight; }
            set {
                viewRectHeight = value;
                OnPropertyChanged("ViewRectHeight");
            }
        }
        #endregion

        #if LOGGING
        private void logEvent( string msg )
        {
            Console.WriteLine(msg);
        }
        #endif 

        /// <summary>
        /// This function is called when the file containing
        /// path and view info is updated.
        /// </summary>
        /// <param name="filename">unused.</param>
        private void onFileChange(string filename)
        {
            #if LOGGING
            logEvent( "* Tracked file Change" );         
            #endif

            using (FileStream fs = File.OpenRead(filename))
            using (TextReader reader = new StreamReader(fs))
            {
                string line = "";

                try { line = reader.ReadLine(); }
                catch (IOException)
                {   /* can happen, not a problem in this case. */
                    #if LOGGING
                        logEvent("|-> IOException while reading wake file");
                    #endif
                    return;
                }
                catch (ArgumentOutOfRangeException)
                { /* we're searching for a PID and a path, nothing big
                   * Ignore if to big */
                    #if LOGGING
                        logEvent("|-> Argument out of range, while reading wake file");
                    #endif
                    Application.Current.Shutdown();
                }

                #if LOGGING
                logEvent( "- Loading : " + line );         
                #endif

                switch (line)
                {
                    case "quit":
                        Application.Current.Shutdown();
                        break;

                    default:
                        string[] infos = line.Split('?');
                        string file = infos[infos.Length - 1];

                        try {
                            ViewRectTop = int.Parse(infos[0]);
                            ViewRectHeight = int.Parse(infos[1]) - ViewRectTop;
                        }
                        catch (System.Exception)
                        {   /* don't care about parsing errors for this one */
                        #if LOGGING
                            logEvent("|-> Exception while parsing INT");
                        #endif
                            ViewRectTop = 0.0;
                            ViewRectHeight = 0.0;
                        }

                        if (!File.Exists(file))
                            return;

                        try
                        {
                            BitmapImage newOverview = new BitmapImage();

                            newOverview.BeginInit();
                            newOverview.CreateOptions = BitmapCreateOptions.IgnoreImageCache;
                            newOverview.CacheOption = BitmapCacheOption.OnLoad;
                            newOverview.UriSource = new Uri(file);
                            newOverview.EndInit();

                            pictureViewer.Source = newOverview;
                        }
                        catch (UriFormatException)
                        {
                        #if LOGGING
                            logEvent("|-> UriFormatException");
                        #endif
                            /* Bad image... putting empty image instead */
                            pictureViewer.Source = null;
                        }
                        InvalidateVisual();
                        break;

                }
            }
        }


        /// <summary>
        /// Given a point in an image, we send keys to vim
        /// in order to update it's view. We use the `count`gg
        /// command to positionate cursor at the given line.
        /// </summary>
        /// <param name="pos">Click position in the image.</param>
        private void updateVimView( Point pos )
        {
            Key[] numbers = { Key.NumPad0, Key.NumPad1, Key.NumPad2
                            , Key.NumPad3, Key.NumPad4, Key.NumPad5
                            , Key.NumPad6, Key.NumPad6, Key.NumPad7
                            , Key.NumPad8, Key.NumPad9
                            };

            string  heightI = ((int)pos.Y + 1).ToString();

            foreach (char c in heightI)
                SendKeyToFollowedProcess( numbers[(int)c - (int)'0'] );

            SendKeyToFollowedProcess(Key.G);
            SendKeyToFollowedProcess(Key.G);

            // we assume that showing width doesn't change
            // and update just the top of our rect
            ViewRectTop = Math.Min( Math.Max(pos.Y - ViewRectHeight / 2, 0.0)
                                  , pictureViewer.ActualHeight - ViewRectHeight);
        }

        /// <summary>
        /// Called when we click on the overview. We use this event
        /// to send messages to vim's window.
        /// </summary>
        /// <param name="sender">should be pictureViewer</param>
        /// <param name="e">mouse param</param>
        private void pictureViewer_MouseLeftButtonUp(object sender, MouseButtonEventArgs e)
            { updateVimView(e.GetPosition((Image)sender)); }

        /// <summary>
        /// Called when mouse move over overview, used to track
        /// dragging.
        /// </summary>
        /// <param name="sender">pictureViewer normaly.</param>
        /// <param name="e"></param>
        private void pictureViewer_MouseMove(object sender, MouseEventArgs e)
        {
            if (e.LeftButton == MouseButtonState.Pressed)
                updateVimView(e.GetPosition((Image)sender));
        }

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;

        protected void OnPropertyChanged( string propName )
        {
            if (this.PropertyChanged != null)
                PropertyChanged(this, new PropertyChangedEventArgs(propName));
        }

        #endregion
    }
}
