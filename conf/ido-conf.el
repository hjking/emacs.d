;;
;; ido
;;
(message "%d: >>>>> Loading [ ido ] Customizations ...." step_no)
(setq step_no (1+ step_no))
(require 'ido)
(ido-mode 1)
;; (setq ido-save-directory-list-file "~/.emacs.d/_ido_last")
(setq ido-save-directory-list-file (concat my-cache-dir "ido_last"))
(setq ido-default-buffer-method 'selected-window)
(setq ido-create-new-buffer 'always)
(setq ido-enable-regexp t)
(setq ido-completion-buffer-all-completions t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-record-commands t)
(setq ido-max-directory-size 100000)
(setq org-completion-us-ido t)
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
(setq ido-max-prospects 7)
(setq ido-enable-tramp-completion nil)
(setq ido-confirm-unique-completion nil)
(setq ido-enable-prefix nil)

(require 'ido-hacks)
(ido-hacks-mode +1)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode +1)
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

(require 'ido-better-flex)
(ido-better-flex/enable)

;; Bind `~` to go to homedir when in ido-find-file; http://whattheemacsd.com/setup-ido.el-02.html
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
