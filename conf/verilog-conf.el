
;; Filename: verilog-conf.el
;; Description: Setting for verilog.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2014-02-10 14:14:55

(use-package verilog-mode
  ; :load-path (lambda () (concat my-site-lisp-dir "verilog-mode/"))
  :mode (("\\.[st]*v[hp]*\\'" . verilog-mode) ; .v, .sv, .svh, .tv, .vp
         ("\\.psl\\'"         . verilog-mode)
         ("\\.vinc\\'"        . verilog-mode))
  :init
   (progn
    (setq verilog-case-fold                nil)
    (setq verilog-auto-arg-sort            t)
    (setq verilog-indent-level             4)   ; 3
    (setq verilog-indent-level-module      4)   ; 3
    (setq verilog-indent-level-declaration 4)   ; 3
    (setq verilog-indent-level-behavioral  4)   ; 3
    (setq verilog-indent-level-directive   2)   ; 1
    (setq verilog-case-indent              4)   ; 2
    (setq verilog-auto-newline             nil) ; t
    (setq verilog-auto-indent-on-newline   t)   ; t
    (setq verilog-tab-always-indent        nil)   ; t
    (setq verilog-auto-endcomments         t)   ; t
    (setq verilog-minimum-comment-distance 40)  ; 10
    (setq verilog-highlight-p1800-keywords nil)
    (setq verilog-indent-begin-after-if    t)   ; t
    ; (setq verilog-auto-lineup              nil) ; 'declarations
    (setq verilog-align-ifelse             t) ; nil
    (setq verilog-tab-to-comment           nil) ; t
    (setq verilog-date-scientific-format   t)   ; t
    (setq verilog-auto-wire-type           "wire")
    ;; Personal
    (setq verilog-company     "Fiberhome Tech (Wuhan) Co.,Ltd")
    (setq verilog-linter      "vcs +v2k -R -PP -Mupdate -P /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/vcsd.tab /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/pli.a +vcsd +vcsd +incdir+.")
    (setq verilog-compiler    "vcs +v2k -R -PP -Mupdate -P /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/vcsd.tab /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/pli.a +vcsd +vcsd +incdir+.")
    (setq verilog-simulator   "vcs +v2k  -R -PP -Mupdate -P /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/vcsd.tab /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/pli.a +vcsd +vcsd +incdir+.")
    (setq verilog-tool        "vcs +v2k  -R -PP -Mupdate -P /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/vcsd.tab /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/pli.a +vcsd +vcsd +incdir+.")
    )
   :config
   (progn
    (add-hook 'verilog-mode-hook '(lambda () (font-lock-mode 1)))

    ;; Convert all tabs in region to multiple spaces
    (add-hook 'verilog-mode-hook
              '(lambda ()
                  (add-hook 'local-write-file-hooks 'hjking/cleanup-buffer-safe)))

    ;; let company support verilog
    ; (add-to-list 'company-keywords-alist (cons 'verilog-mode verilog-keywords))
    )
  )

(provide 'verilog-conf)