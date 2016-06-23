
;; Filename: vlog-conf.el
;; Description: Setting for vlog-mode.el
;; Author: Hong Jin
;; Created: 2010-12-22 10:00
;; Last Updated: 2014-02-10 14:15:35

(use-package vlog-mode
  :load-path (lambda () (concat my-site-lisp-dir "vlog-mode/"))
  :diminish "Verilog"
  :mode (("\\.[st]*v[hp]*\\'" . vlog-mode) ; .v, .sv, .svh, .tv, .vp
         ("\\.psl\\'"         . vlog-mode)
         ("\\.vinc\\'"        . vlog-mode))
  :init
  (setq vlog-mode-highlight-all-uppercase-words t)

  (setq vlog-align-mod-inst-stop-list '(28 52))

  (setq vlog-indent-level-beh                4
        vlog-indent-level-block              0
        vlog-indent-level-block-beh          0
        vlog-indent-level-block-inside       4
        vlog-indent-level-case-inside        4
        vlog-indent-level-case-branch-inside 4
        vlog-indent-level-cond               4
        vlog-indent-level-default            4
        vlog-indent-level-port-list          4
        vlog-indent-align-port-list-to-paren t
        vlog-indent-align-else-to-if         nil)

  (setq vlog-skel-header-string "\
  // ----------------------------------------------------------------------
  //  Copyright (c) %<time %Y>, %<company>
  //  Microelectronics Dept. Verification Group.
  //  All rights reserved.
  //
  //  File     : %<filename>
  //  Author   : %<author>
  //  EMail    : hongjin@fiberhome.com.cn
  //  Created  : %<time %Y-%m-%d %02H:%02M:%02S>
  //  Last Changed : %<time %Y-%m-%d %02H:%02M:%02S>
  //  Description : %<_>
  // ----------------------------------------------------------------------
  //  History:
  //      Author   : %<author>
  //      Date     : %<time %Y-%m-%d %02H:%02M:%02S>
  //      Revision : 1.0
  // ----------------------------------------------------------------------\n")

  (setq vlog-skel-user-name    "Hong Jin"
        vlog-skel-company-name "Fiberhome Telecommunication Technology Co., Ltd.")

  :config
  (setq vlog-mode-keywordset-docs
      (append vlog-mode-keywordset-docs
              (list "Note:" "NOTE:" "note:")))
  (vlog-mode-enable-v2k)
  (vlog-mode-make-keywords)
  ;; Convert all tabs in region to multiple spaces
  (add-hook 'vlog-mode-hook
            '(lambda ()
                (add-hook 'local-write-file-hooks 'hjking/cleanup-buffer-safe)))
  )

(provide 'vlog-conf)