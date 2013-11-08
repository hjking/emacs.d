
;;
;;
;; =====================================================================
;; USEFUL NOTES AND OTHER STUFF
;; =====================================================================

;; == How to record and display a keyboard macro ==

;; Just open a buffer and type "C-x (" Then start typing in your macro.
;; Once you are finished defining your macro type "C-x )" Then type M-x
;; name-last-kbd-macro. This will allow you to call your macro
;; whatever you want. Next open up your .emacs file and position your
;; cursor where you want the code for the macro to appear.  Type M-x
;; insert-kbd-macro and type in the name.  The code will automatically
;; be generated.
;; "C-x e" : execute keyboard macro
;; "C-u 37 C-x e" : execute 37 times
;; =====================================================================

;; Use shell-command-on-region M-| to send region to external
;; process. If you use a prefix argument , C-u M-| this will replace
;; the region with the output of the external process. Good for
;; sending something to stdin and reading from stdout.

;; =====================================================================
;; C-x z : repeat
;; To copy to named register: C-x r s a - Where a is the name of the
;; register ( a - z ) to save the text to.

;; To paste from named register: C-x r i a - Where a is the name of
;; the register ( a - z ) to paste the saved text from.

;; To remember current point: C-x r spc a - Where a is the name of the
;; register to save point to.

;; To jump to named point: C-x r j a - Where a is the name of the
;; register holding desired point to jump to

;; =====================================================================
;; Working with balanced expressions
;; C-M-f    Move forward over a balanced expression
;; C-M-b    Move backward over a balanced expression
;; C-M-k    Kill a balanced expression forward
;; C-M-SPC  Put the mark at the end of the sexp
;; C-M-n    Move forward over a parenthetical group
;; C-M-p    Move backward over a parenthetical group
;; =====================================================================

;;  For Windows users
;;
;;    - HOME variable
;;
;;      Don't put a trailing backslash at the end of your `HOME'
;;      variable (if you have any). Otherwise, `~/emacs.d/server'
;;      gets (for example) translated to `Z://emacs.d/server' (double
;;      slash)!
;;
;;    - PATH variable
;;
;;      Put `C:\cygwin\bin' in the first place, in order to select the correct
;;      `find' command (for searches).

;; require一般都是一些插件默认的加载模式，如果用这个加载，有可能不能使用； 
;; load，auto-load，这两个的区别就是：
;; load是把整个插件加载进来，
;; 而auto-load只是把插件的function表加载进来，等你使用的时候在加载程序模块 
;; load-library一般都是在M-x后使用，比较适合作自己编辑的模块的加载 
;;
;;
;; c-x 4 f open a new file in a split window
