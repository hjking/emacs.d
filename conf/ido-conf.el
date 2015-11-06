;;
;; ido
;;
(message "%d: >>>>> Loading [ ido ] Customizations ...." step_no)
(setq step_no (1+ step_no))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(ido-mode 1)
; (setq ido-everywhere t)
(ido-everywhere 1)
;; (setq ido-save-directory-list-file "~/.emacs.d/_ido_last")
(setq ido-save-directory-list-file (concat my-cache-dir "ido_last"))
;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)
(setq ido-create-new-buffer 'always)
(setq ido-enable-regexp t)
(setq ido-completion-buffer-all-completions t)
(setq ido-enable-flex-matching t)
(setq ido-record-commands t)
(setq ido-max-directory-size 100000)
(setq ido-auto-merge-work-directories-length nil)
;; Show previously opened buffers in ido-switch-buffer
(setq ido-use-virtual-buffers t)
(setq ido-case-fold t)
(setq ido-enable-last-directory-history t)
(setq ido-buffer-history t)
(setq ido-max-work-directory-list 100)
(setq ido-max-work-file-list 50)
;; If thing-at-point looks like something openable, default to it
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point t)
(setq ido-max-prospects 10)
(setq ido-enable-tramp-completion nil)
(setq ido-confirm-unique-completion nil)
(setq ido-enable-prefix nil)
;;
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
(setq ido-default-file-method 'selected-window)

;;; Set buffer separator in the mini buffer when press C-x b (ido-switch-buffer)
;;; to new line instead of the character | so that it can be easy to read
(setq ido-decorations
      '("\n=> " "" "\n" "" "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (require 'ido-hacks)
; (ido-hacks-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use ido everywhere
;; not enabled for org mode or magit mode
;; get rid of compile warning
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar inherit-input-method nil)
(defvar predicate nil)
(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try out flx-ido for better flex matching between words
;; provides fuzzy completion to select completion candidates.
;; For example, if you want to select a file src/foo/bar.txt,
;; you only need to type in Ido prompt "sfb", short for (s)rc/(f)oo/(b)ar
(defvar flx-ido-mode nil)
(use-package flx-ido
  :config (flx-ido-mode +1))
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Bind `~` to go to homedir when in ido-find-file;
;; http://whattheemacsd.com/setup-ido.el-02.html
(add-hook 'ido-setup-hook
  (lambda ()
    ;; Go straight home
    (define-key ido-file-completion-map
      (kbd "~")
      (lambda ()
        (interactive)
        (if (looking-back "/")
            (insert "~/")
            (call-interactively 'self-insert-command))))))

(add-hook 'ido-setup-hook 'fc/ido-setup)
(defun fc/ido-setup ()
  (define-key ido-common-completion-map (kbd "C-c")
    (make-sparse-keymap))
  (define-key ido-common-completion-map (kbd "C-c C-u")
    'fc/ido-copy-selection)
  (define-key ido-file-dir-completion-map (kbd "<up>")
    'ido-prev-work-directory)
  (define-key ido-file-dir-completion-map (kbd "<down>")
    'ido-next-work-directory))

(defun fc/ido-copy-selection ()
  "Copy the current ido selection to the kill ring."
  (interactive)
  (kill-new (abbreviate-file-name (concat ido-current-directory
            ido-text))))

; (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
; (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
; (defun ido-sort-mtime ()
;     (setq ido-temp-list
;           (sort ido-temp-list
;                 (lambda (a b)
;                   (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
;                         (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
;                     (if (= (nth 0 ta) (nth 0 tb))
;                         (> (nth 1 ta) (nth 1 tb))
;                       (> (nth 0 ta) (nth 0 tb)))))))
;     (ido-to-end  ;; move . files to end (again)
;      (delq nil (mapcar
;                 (lambda (x) (if (string-equal (substring x 0 1) ".") x))
;                 ido-temp-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-vertical-mode
;; Makes ido-mode display vertically.
(use-package ido-vertical-mode
  :config
  (progn
   (ido-vertical-mode)
   (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-at-point
;; an alternative frontend for `completion-at-point'
; (require 'ido-at-point)
; (ido-at-point-mode)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ido-conf)