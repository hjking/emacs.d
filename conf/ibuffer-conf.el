
;; Filename: ibuffer-conf.el
;; Description: Setting for ibuffer.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2013-09-23 20:03:06
;;
(message "%d: >>>>> Loading [ ibuffer ] Customizations File ...." step_no)
(setq step_no (1+ step_no))
(require 'ibuffer)
(require 'ibuf-ext nil t)
(setq ibuffer-delete-window-on-quit t)
;; replaces the functionality of list-buffers command
(defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "C-x C-b")   'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(when (featurep 'ibuffer)
  (define-key ibuffer-mode-map "r"  'ywb-ibuffer-rename-buffer)
  ;; (define-key ibuffer-mode-map (kbd "C-x C-f")  'ywb-ibuffer-find-file)
  (define-key ibuffer-mode-map " "  'scroll-up)


  (setq ibuffer-saved-filters
      '(("t" ((or (mode . latex-mode)
                  (mode . plain-tex-mode))))
        ("c" ((or (mode . c-mode)
                  (mode . c++-mode))))
        ("p" ((mode . cperl-mode)))
        ("e" ((or (mode . emacs-lisp-mode)
                  (mode . lisp-interaction-mode))))
        ("d" ((mode . dired-mode)))
        ("s" ((mode . shell-mode)))
        ("i" ((mode . image-mode)))
        ("h" ((mode . html-mode)))
        ("emacs" (or
             (name . "^\\*scratch\\*$")
             (name . "^\\*Messages\\*$")))
        ("gnus" ((or (mode . message-mode)
                     (mode . mail-mode)
                     (mode . gnus-group-mode)
                     (mode . gnus-summary-mode)
                     (mode . gnus-article-mode))))
        ("pr" ((or (mode . emacs-lisp-mode)
                   (mode . cperl-mode)
                   (mode . c-mode)
                   (mode . c++-mode)
                   (mode . php-mode)
                   (mode . java-mode)
                   (mode . idl-mode)
                   (mode . lisp-interaction-mode))))
        ("m" ((mode . muse-mode)))
        ("w" ((or (mode . emacs-wiki-mode)
                  (mode . muse-mode))))
        ("*" ((name . "*")))
        ))
  )

;;;###autoload
(defun ywb-ibuffer-rename-buffer ()
  (interactive)
  (call-interactively 'ibuffer-update)
  (let* ((buf (ibuffer-current-buffer))
         (name (generate-new-buffer-name
                (read-from-minibuffer "Rename buffer(to new name): "
                                      (buffer-name buf)))))
    (with-current-buffer buf
      (rename-buffer name)))
  (call-interactively 'ibuffer-update))

(defun ywb-ibuffer-find-file ()
  (interactive)
  (let ((default-directory (let ((buf (ibuffer-current-buffer)))
      (if (buffer-live-p buf)
        (with-current-buffer buf
          default-directory)
        default-directory))))
    (call-interactively 'ido-find-file)))

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Define 3 formats, each including the new mk-proj-col
;; column. Switch formats with ibuffer-switch-format (bound to "`").
;; use 'size-h instead of 'size
(setq ibuffer-formats
    '((mark modified read-only
            "  "
            (name 18 18 :left :elide)
            " "
            (size-h 9 -1 :right)
            " "
            (mode 16 16 :left :elide)
            " "
            (git-status 8 8 :left)
            " "
            filename-and-process)
    (mark modified read-only
            (name 45 -1 :left)
            " "
            filename-and-process)
    (mark modified read-only
            filename-and-process)))

(setq ibuffer-elide-long-columns t)
(setq ibuffer-eliding-string "&")

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
    "Open ibuffer with cursor pointed to most recent buffer name"
    (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)



(setq ibuffer-saved-filter-groups
                    (quote (("default"
                      ("*buffer*"   (name . "\\*.*\\*"))
                      ("Dirs"       (mode . dired-mode))
                      ("Shell"      (mode . shell-script-mode))
                      ("Verilog"    (mode . verilog-mode))
                      ("C"          (or (mode . c-mode)
                                        (mode . c++-mode)))
                      ("Elisp"      (or (mode . emacs-lisp-mode)
                                        (mode . lisp-interaction-mode)))
                      ("Perl"       (mode . cperl-mode))
                      ("Python"     (mode . python-mode))
                      ("Org"        (or (mode . org-mode)
                                        (mode . org-agenda-mode)))
                      ("Music"      (name . "^EMMS Music Playlist$"))
                      ("Tags"       (name . "^TAGS\\(<[0-9]+>\\)?$"))
                      ("IRC"        (mode . erc-mode))
                      ))))
(add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))
  ; (add-hook 'ibuffer-mode-hook (lambda ()
  ;             (setq ibuffer-filter-groups
  ;                   '(
  ;                     ("*buffer*"   (name . "\\*.*\\*"))
  ;                     ("Dirs"       (mode . dired-mode))
  ;                     ("Shell"      (mode . shell-script-mode))
  ;                     ("Verilog"    (mode . verilog-mode))
  ;                     ("C"          (or (mode . c-mode)
  ;                                       (mode . c++-mode)))
  ;                     ("Elisp"      (or (mode . emacs-lisp-mode)
  ;                                       (mode . lisp-interaction-mode)))
  ;                     ("Perl"       (mode . cperl-mode))
  ;                     ("Python"     (mode . python-mode))
  ;                     ("Org"        (or (mode . org-mode)
  ;                                       (mode . org-agenda-mode)))
  ;                     ("Music"      (name . "^EMMS Music Playlist$"))
  ;                     ("Tags"       (name . "^TAGS\\(<[0-9]+>\\)?$"))
  ;                     ("IRC"        (mode . erc-mode))
  ;                     ))))


;;
;; ibuffer-git
;;
(require 'ibuffer-git nil t)
