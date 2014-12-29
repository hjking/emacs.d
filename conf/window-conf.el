
(message "%d: >>>>> Loading [ Windows ] Customization ...." step_no)
(setq step_no (1+ step_no))

;; [ winner ]----------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
(winner-mode 1)
;; copied from http://puntoblogspot.blogspot.com/2011/05/undo-layouts-in-emacs.html
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)


;; [ windmove ]-------------------------------------------------------------
(message "    >>>>> Loading [ windmove ] Customizations ....")
; (require 'windmove)
; (global-set-key (kbd "C-<f2> <left>")  'windmove-left)
; (global-set-key (kbd "C-<f2> <right>") 'windmove-right)
; (global-set-key (kbd "C-<f2> <up>")    'windmove-up)
; (global-set-key (kbd "C-<f2> <down>")  'windmove-down)
(use-package windmove
  :bind
  (("C-<f2> <right>" . windmove-right)
   ("C-<f2> <left>"  . windmove-left)
   ("C-<f2> <up>"    . windmove-up)
   ("C-<f2> <down>"  . windmove-down)))
;; use Meta key as prefix key
;;  (windmove-default-keybindings 'meta)
;; [ windmove ]----------------------------------------------------[ End ]--


;; buffer-move.el
;; Swap buffers without typing C-x b on each window
(require 'buffer-move)
(global-set-key (kbd "C-c C-b C-k")     'buf-move-up)
(global-set-key (kbd "C-c C-b C-j")   'buf-move-down)
(global-set-key (kbd "C-c C-b C-h")   'buf-move-left)
(global-set-key (kbd "C-c C-b C-l")  'buf-move-right)


;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
    (lexical-let ((s-f split-function))
    (lambda ()
        (interactive)
        (funcall s-f)
        (set-window-buffer (next-window) (other-buffer)))))

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
    (interactive)
    (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
    (interactive)
    (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)


;; --[ Switch Window ]----------------------------------------------------------
; (message "    >>>>> Loading [ Switch Window ] Customization ....")
; (add-site-lisp-load-path "switch-window/")
; (require 'switch-window)
; (global-set-key (kbd "C-x o") 'switch-window)
;; --------------------------------------------------------------------[ End ]--

;;--------------- ace-window -----------------------------------------
(add-site-lisp-load-path "ace-window/")
(require 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(global-set-key (kbd "C-x o") 'ace-window)

;;---------------------------------------------------------------------
; +----------+-----------+
; |          |           |
; |          |           |
; +----------+-----------+
; |          |           |
; |          |           |
; +----------+-----------+
(defun split-window-4()
  "Splite window into 4 sub-window"
  (interactive)
  (if (= 1 (length (window-list)))
      (progn (split-window-vertically)
       (split-window-horizontally)
       (other-window 2)
       (split-window-horizontally)
       )
  ))

;  +----------------------+                 +------------+-----------+
;  |                      |           \     |            |           |
;  |                      |   +-------+\    |            |           |
;  +----------+-----------+   +-------+/    |            +-----------+
;  |          |           |           /     |            |           |
;  |          |           |                 |            |           |
;  +----------+-----------+                 +------------+-----------+

(defun split-v-3 ()
  "Change 3 window style from horizontal to vertical"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
      (let ((1stBuf (window-buffer (car winList)))
      (2ndBuf (window-buffer (car (cdr winList))))
      (3rdBuf (window-buffer (car (cdr (cdr winList))))))
        (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
        (delete-other-windows)
        (split-window-horizontally)
        (set-window-buffer nil 1stBuf)
        (other-window 1)
        (set-window-buffer nil 2ndBuf)
        (split-window-vertically)
        (set-window-buffer (next-window) 3rdBuf)
        (select-window (get-largest-window))
    ))))

;  +------------+-----------+                  +----------------------+
;  |            |           |            \     |                      |
;  |            |           |    +-------+\    |                      |
;  |            +-----------+    +-------+/    +----------+-----------+
;  |            |           |            /     |          |           |
;  |            |           |                  |          |           |
;  +------------+-----------+                  +----------+-----------+


(defun split-h-3 ()
  "Change 3 window style from vertical to horizontal"
  (interactive)

  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
      (let ((1stBuf (window-buffer (car winList)))
      (2ndBuf (window-buffer (car (cdr winList))))
      (3rdBuf (window-buffer (car (cdr (cdr winList))))))
     (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)

     (delete-other-windows)
     (split-window-vertically)
     (set-window-buffer nil 1stBuf)
     (other-window 1)
     (set-window-buffer nil 2ndBuf)
     (split-window-horizontally)
     (set-window-buffer (next-window) 3rdBuf)
     (select-window (get-largest-window))
        ))))

;  +------------+-----------+                 +------------+-----------+
;  |            |           |            \    |            |           |
;  |            |           |    +-------+\   |            |           |
;  +------------+-----------+    +-------+/   +------------+           |
;  |                        |            /    |            |           |
;  |                        |                 |            |           |
;  +------------+-----------+                 +------------+-----------+
;  +------------+-----------+                 +------------+-----------+
;  |            |           |            \    |            |           |
;  |            |           |    +-------+\   |            |           |
;  |            +-----------+    +-------+/   +------------+-----------+
;  |            |           |            /    |                        |
;  |            |           |                 |                        |
;  +------------+-----------+                 +------------+-----------+

(defun change-split-type-3 ()
  "Change 3 window style from horizontal to vertical and vice-versa"
  (interactive)

  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
            (let ((1stBuf (window-buffer (car winList)))
                  (2ndBuf (window-buffer (car (cdr winList))))
                  (3rdBuf (window-buffer (car (cdr (cdr winList)))))

                  (split-3
                   (lambda(1stBuf 2ndBuf 3rdBuf split-1 split-2)
                     "change 3 window from horizontal to vertical and vice-versa"
                     (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)

                     (delete-other-windows)
                     (funcall split-1)
                     (set-window-buffer nil 2ndBuf)
                     (funcall split-2)
                     (set-window-buffer (next-window) 3rdBuf)
                     (other-window 2)
                     (set-window-buffer nil 1stBuf)))

                  (split-type-1 nil)
                  (split-type-2 nil)
                  )
              (if (= (window-width) (frame-width))
                  (setq split-type-1 'split-window-horizontally
                        split-type-2 'split-window-vertically)
                (setq split-type-1 'split-window-vertically
           split-type-2 'split-window-horizontally))
              (funcall split-3 1stBuf 2ndBuf 3rdBuf split-type-1 split-type-2)
  ))))

(global-set-key (kbd "C-x 4 c") (quote change-split-type-3))


;  +------------+-----------+                   +------------+-----------+
;  |            |     C     |            \      |            |     A     |
;  |            |           |    +-------+\     |            |           |
;  |     A      |-----------|    +-------+/     |     B      |-----------|
;  |            |     B     |            /      |            |     C     |
;  |            |           |                   |            |           |
;  +------------+-----------+                   +------------+-----------+
;
;  +------------------------+                   +------------------------+
;  |           A            |           \       |           B            |
;  |                        |   +-------+\      |                        |
;  +------------+-----------+   +-------+/      +------------+-----------+
;  |     B      |     C     |           /       |     C      |     A     |
;  |            |           |                   |            |           |
;  +------------+-----------+                   +------------+-----------+

(defun roll-v-3 (&optional arg)
    "Rolling 3 window buffers (anti-)clockwise"
    (interactive "P")
    (select-window (get-largest-window))
    (if (= 3 (length (window-list)))
        (let ((winList (window-list)))
          (let ((1stWin (car winList))
                (2ndWin (car (cdr winList)))
                (3rdWin (car (last winList))))
            (let ((1stBuf (window-buffer 1stWin))
                  (2ndBuf (window-buffer 2ndWin))
                  (3rdBuf (window-buffer 3rdWin)))
              (if arg (progn
 ; anti-clockwise
                        (set-window-buffer 1stWin 3rdBuf)
                        (set-window-buffer 2ndWin 1stBuf)
                        (set-window-buffer 3rdWin 2ndBuf))
                (progn                                      ; clockwise
                  (set-window-buffer 1stWin 2ndBuf)
                  (set-window-buffer 2ndWin 3rdBuf)
                  (set-window-buffer 3rdWin 1stBuf))
                ))))))

(global-set-key (kbd "C-x 4 r")  (quote roll-v-3))

(provide 'window-conf)