
(message "%d: >>>>> Loading [ Windows Customization ] ...." step_no)
(setq step_no (1+ step_no))

;; [ winner ]----------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
(winner-mode 1)
;; copied from http://puntoblogspot.blogspot.com/2011/05/undo-layouts-in-emacs.html
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)

;; [ windmove ]-------------------------------------------------------------
(message "    >>>>> Loading [ windmove ] Customizations ....")
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
;; (require 'buffer-move)
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
(message "%d: >>>>> Loading [ Switch Window Customization ] ...." step_no)
(setq step_no (1+ step_no))
(add-site-lisp-load-path "switch-window/")
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
;; --------------------------------------------------------------------[ End ]--
