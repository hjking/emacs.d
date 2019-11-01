
;; Filename: ibuffer-conf.el
;; Description: Setting for ibuffer.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2013-12-31 15:33:39
;;

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
   ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
  -^----------^-+-^----^--------+-^-------^--------+-^----^-------
    _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
   _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
    _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
  -^----------^-+-^----^--------+-^-------^--------+-^----^-------
  "
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" ibuffer-quit "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                                     :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                       :after-exit
                                       (if (eq major-mode 'ibuffer-mode)
                                           (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))


(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b"  .  ibuffer)
  :init
    (progn
      (defalias 'list-buffers 'ibuffer)
      (setq ibuffer-default-sorting-mode 'major-mode)
      (setq ibuffer-elide-long-columns t)
      (setq ibuffer-eliding-string "&")
      (setq ibuffer-filter-group-name-face 'font-lock-doc-face)
      (setq ibuffer-delete-window-on-quit t)
      (setq ibuffer-expert t)
      (setq ibuffer-show-empty-filter-groups nil)
      (setq ibuffer-display-summary nil)
      (setq ibuffer-formats
            '((mark modified read-only
                    " "
                    (name 18 18 :left :elide)
                    " "
                    (size-h 9 -1 :right)
                    " "
                    (mode 16 16 :left :elide)
                    " "
                    ; (vc-status 16 16 :left)
                    ; " "
                    filename-and-process)
              (mark modified read-only
                    (name 45 -1 :left)
                    " "
                    filename-and-process)
              (mark modified read-only
                    filename-and-process)))
      ;; grouping
      (setq ibuffer-saved-filter-groups
            '(("default"
              ("emacs"      (name . "\\*.*\\*"))
              ("Dirs"       (mode . dired-mode))
              ("Shell"      (or (mode . term-mode)
                                (mode . eshell-mode)
                                (mode . shell-mode)))
              ("HDL"        (or (mode . verilog-mode)
                                (mode . vhdl-mode)
                                (mode . vlog-mode)))
              ("C"          (or
                             (mode . c-mode)
                             (mode . cc-mode)
                             (mode . c++-mode)))
              ("Elisp"      (or
                             (mode . emacs-lisp-mode)
                             (mode . lisp-interaction-mode)))
              ("Perl"       (mode . cperl-mode))
              ("Python"     (mode . python-mode))
              ("Org"        (or
                             (name . "^\\*Calendar\\*$")
                             (name . "^diary$")
                             (mode . org-mode)
                             (mode . org-agenda-mode)))
              ("Music"      (name . "^EMMS Music Playlist$"))
              ("Tags"       (name . "^TAGS\\(<[0-9]+>\\)?$"))
              ("IRC"        (mode . erc-mode))
              ("Markdown"
                           (or
                            (mode . markdown-mode)))
              ("Web"          (or
                             (mode . css-mode)
                             (mode . web-mode)))
      )))
    )
  :config (progn
    ;; Use human readable Size column instead of original one
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))
    (key-chord-define ibuffer-mode-map "hh" #'hydra-ibuffer-main/body)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'ibuffer-conf)