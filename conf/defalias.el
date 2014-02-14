
(message "%d: >>>>> Loading [ Aliases ] ...." step_no)
(setq step_no (1+ step_no))

(when (>= 21 emacs-major-version)
  (defalias 'move-beginning-of-line 'beginning-of-line)
  (defalias 'move-end-of-line       'end-of-line))

(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough

(defalias 'man 'woman)

; shortening of often used commands
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'sl 'sort-lines)
(defalias 'rr 'reverse-region)
(defalias 'rs 'replace-string)

(defalias 'g 'grep)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)

(defalias 'rb 'revert-buffer)

(defalias 'sh 'shell)
(defalias 'ps 'powershell)
(defalias 'fb 'flyspell-buffer)
(defalias 'sbc 'set-background-color)
(defalias 'rof 'recentf-open-files)
(defalias 'lcd 'list-colors-display)
(defalias 'cc 'calc)

; elisp
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'eis 'elisp-index-search)
(defalias 'lf 'load-file)

; major modes
(defalias 'hm 'html-mode)
(defalias 'tm 'text-mode)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'om 'org-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'html6-mode 'xah-html6-mode)

; minor modes
(defalias 'wsm 'whitespace-mode)
(defalias 'gwsm 'global-whitespace-mode)
(defalias 'dsm 'desktop-save-mode)
(defalias 'acm 'auto-complete-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'glm 'global-linum-mode)

; major modes, easy naming
(defalias 'ahk-mode 'xahk-mode)
(defalias 'bbcode-mode 'xbbcode-mode)
(defalias 'cmd-mode 'dos-mode)
(defalias 'lsl-mode 'xlsl-mode)
(defalias 'ocaml-mode 'tuareg-mode)
(defalias 'math-symbol-input-mode 'xmsi-mode)

;; xah personal
(defalias '8w 'xwe-new-word-entry)
(defalias '8d 'xwe-add-definition)
(defalias '8s 'xwe-add-source)
(defalias 'c 'xah-cite)
(defalias 'cr 'compact-region)
(defalias 'cw 'count-words-region-or-line)
(defalias 'db 'dehtmlize-block)
(defalias 'dr 'dehtmlize-region)
(defalias 'dsc 'delete-secondlife-cache)
(defalias 'dss 'desktop-settings-setup)
(defalias 'dstp 'xah-fix-dstp)
(defalias 'eol 'replace-eols-to-p)
(defalias 'fs 'full-size-img-linkify)
(defalias 'ft 'fix-timestamp)
(defalias 'hb 'htmlize-block)
(defalias 'il 'image-linkify)
(defalias 'irh 'insert-random-hex)
(defalias 'irs 'insert-random-string)
(defalias 'k 'htmlize-keyboard-shortcut-notation)
(defalias 'wl 'wikipedia-linkify)
(defalias 'l 'xah-file-linkify)
(defalias 'lb 'listify-block)
(defalias 'mb 'make-blogger-entry)
(defalias 'mht 'make-html-table)
(defalias 'tb 'title-bracket-to-html-tag)
(defalias 'tls 'toggle-line-spacing)
(defalias 'xmae 'xah-make-atom-entry)
(defalias 'z 'amazon-linkify)
(defalias '~ 'make-backup)

(defalias '\(1\) 'xah-fix-number-items-block)
(defalias '& 'replace-html-chars)
(defalias '\\ 'escape-quotes-region)
(defalias '\[ 'remove-square-brackets)
(defalias '\" 'replace-straight-quotes)
