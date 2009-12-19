using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace WpfOverview
{
    class WinSys
    {
        public const UInt32 WM_KEYDOWN = 0x0100;
        public const UInt32 WM_KEYUP = 0x0101;
        public const UInt32 SW_HIDE =             0;
        public const UInt32 SW_SHOWNORMAL =       1;
        public const UInt32 SW_NORMAL =           1;
        public const UInt32 SW_SHOWMINIMIZED =    2;
        public const UInt32 SW_SHOWMAXIMIZED =    3;
        public const UInt32 SW_MAXIMIZE =         3;
        public const UInt32 SW_SHOWNOACTIVATE =   4;
        public const UInt32 SW_SHOW =             5;
        public const UInt32 SW_MINIMIZE =         6;
        public const UInt32 SW_SHOWMINNOACTIVE =  7;
        public const UInt32 SW_SHOWNA =           8;
        public const UInt32 SW_RESTORE =          9;
        public const UInt32 SW_SHOWDEFAULT =      10;
        public const UInt32 SW_FORCEMINIMIZE =    11;
        public const UInt32 SW_MAX =              11;

        [DllImport("user32.dll")]
        public static extern bool SetWindowPos(IntPtr hWnd, IntPtr hWndInsertAfter, int X, int Y, int cx, int cy, uint uFlags);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool GetWindowRect(IntPtr hWnd, ref RECT lpRect);

        [return: MarshalAs(UnmanagedType.Bool)]
        [DllImport("user32.dll", SetLastError = true)]
        public static extern bool PostMessage(IntPtr hWnd, UInt32 Msg, Int32 wParam, Int32 lParam);

        [DllImport("user32.dll")]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool GetWindowPlacement(IntPtr hWnd, ref WINDOWPLACEMENT lpwndpl);

        [DllImport("user32.dll")]
        public static extern bool SetWindowPlacement(IntPtr hWnd, [In] ref WINDOWPLACEMENT lpwndpl);

        [DllImport("user32.dll")]
        public static extern int ShowWindow(IntPtr hWnd, UInt32 nCmdShow);

        [StructLayout(LayoutKind.Sequential)]
        public struct WINDOWPLACEMENT
        {
            public UInt32 length;
            public UInt32 flags;
            public UInt32 showCmd;
            public POINT ptMinPosition;
            public POINT ptMaxPosition;
            public RECT rcNormalPosition;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct POINT
        {
            public int x;
            public int y;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct RECT
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
    }
}
