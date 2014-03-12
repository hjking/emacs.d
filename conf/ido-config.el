(require 'ido-at-point)

(defun ido-at-point-x-prompt (prompt choices &optional display-fn)
  "Display choices in a x-window prompt."
  (when (and window-system choices)
    (let ((chosen
           (let (menu d) ;; d for display
             (dolist (c choices)
               (setq d (or (and display-fn (funcall display-fn c))
                           c))
               (cond ((stringp d)
                      (push (cons (concat "   " d) c) menu))
                     ((listp d)
                      (push (car d) menu))))
             (setq menu (list prompt (push "title" menu)))
             (x-popup-menu (if (fboundp 'posn-at-point)
                               (let ((x-y (posn-x-y (posn-at-point (point)))))
                                 (list (list (+ (car x-y) 10)
                                             (+ (cdr x-y) 20))
                                       (selected-window)))
                             t)
                           menu))))
      (or chosen
          (keyboard-quit)))))

(defun ido-at-point-read (completions common)
  (ido-at-point-x-prompt "Completions" completions))
  
(ido-at-point-mode t)