namespace WinFormOverview
{
    partial class ViewWindow
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.viewBox = new System.Windows.Forms.PictureBox();
            ((System.ComponentModel.ISupportInitialize)(this.viewBox)).BeginInit();
            this.SuspendLayout();
            // 
            // viewBox
            // 
            this.viewBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.viewBox.Location = new System.Drawing.Point(0, 0);
            this.viewBox.Margin = new System.Windows.Forms.Padding(0);
            this.viewBox.Name = "viewBox";
            this.viewBox.Size = new System.Drawing.Size(97, 309);
            this.viewBox.TabIndex = 0;
            this.viewBox.TabStop = false;
            // 
            // ViewWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(97, 309);
            this.Controls.Add(this.viewBox);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "ViewWindow";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.Text = "CodeOverview";
            ((System.ComponentModel.ISupportInitialize)(this.viewBox)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.PictureBox viewBox;
    }
}

