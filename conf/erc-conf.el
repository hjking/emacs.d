
;; Filename: erc-conf.el
;; Description: Setting for erc.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2012-04-19 17:23:16
;;
(message "%d: >>>>> Loading [ ERC ] Setup ...." step_no)
(setq step_no (1+ step_no))

;; ERC, an Emacs IRC client
(when (require 'erc nil t)
  (autoload 'erc-select-read-args "erc" nil nil) ; needed for XEmacs
  (autoload 'erc-select "erc" nil t)
  (autoload 'erc-select-ssl "erc" nil t)
  (setq erc-default-coding-system '(utf-8 . utf-8))
  ;; set coding seperatly for some channel
  (setq erc-encoding-coding-alist '(("#linuxfire" . chinese-iso-8bit)))
  (setq erc-server                         "irc.freenode.net"
        erc-port                           6667
        erc-user-full-name                 "King Hung"
        erc-email-userid                   "hon9jin@gmail.com"
        erc-nick                           '("hjking" "kinghom" "hon9jin")
        erc-password                       nil ; set this in local config
        erc-nickserv-passwords             nil ; set this in local config
        erc-anonymous-login                t
        erc-auto-query                     'bury
        erc-join-buffer                    'bury
        erc-prompt                         ">"
        erc-max-buffer-size                30000
        erc-prompt-for-password            nil
        erc-join-buffer                    'buffer
        erc-command-indicator              "CMD"
        erc-echo-notices-in-current-buffer t
        erc-send-whitespace-lines          nil
        erc-hide-list                      '("JOIN" "PART" "QUIT")
        erc-ignore-list                    '("jibot"))

  (setq erc-quit-reason-various-alist
        '(("brb"    "I'll be right back.")
          ("lunch"  "Having lunch.")
          ("dinner" "Having dinner.")
          ("food"   "Getting food.")
          ("sleep"  "Sleeping.")
          ("work"   "Getting work done.")
          (".*"     (yow))))

  ;; auto join channels after logined in
  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#org-mode" "#emacs" "#hacklabto" "#vim")
          ("oftc.net" "#debian-zh" "#emacs-cn")
          ("irc.int.ru" "#unix")
          ("irc.fu-berlin.de" "#unix.ru" "#fidorus")
          ("fu-berlin.de" "#unix.ru" "#fidorus")
          ))

  (setq erc-part-reason-various-alist erc-quit-reason-various-alist
        erc-part-reason               'erc-part-reason-various
        erc-quit-reason               'erc-quit-reason-various)
  (defvar ted-erc-autojoin t
    "Whether or not ERC should autojoin on connect.")

  (defvar ted-erc-identify t
    "Whether or not ERC should identify with NickServ on connect.")

  (defun hjking-freenode (autojoin)
    (interactive (list (y-or-n-p "Autojoin channels? ")))
    (setq ted-erc-autojoin autojoin
          ted-erc-identify nil)
    (erc :server "irc.freenode.net"
         :port 8001
         :nick "hjking"
         :password (cdr (assoc "hjking" (cadr (assoc 'freenode erc-nickserv-passwords))))))

  (defun kinghom-gitter-irc ()
    (interactive)
    (erc :server "irc.gitter.im" :port 6667 :nick "kinghom"))

  ;; highlight keywords or users which i'm interested
  (erc-match-mode 1)
  (setq erc-keywords '("emms" "python" "emacs" "org" "vim"))
  (setq erc-pals '("rms"))

  ;; reject some keywords and users
  (setq erc-ignore-list nil)
  (setq erc-hide-list
      '("JOIN" "PART" "QUIT" "MODE"))

  ;; timestamp
  (erc-timestamp-mode 1)

  ;; log
  (require 'erc-log)
  (erc-log-mode 1)
  (setq erc-log-channels t)
  (setq erc-log-channels-directory (concat my-cache-dir "irclogs/"))
  (setq erc-save-buffer-on-part t)
  (setq erc-log-file-coding-system 'utf-8)
  (setq erc-log-write-after-send t)
  (setq erc-log-write-after-insert t)
  (setq erc-log-insert-log-on-open nil)
  (setq erc-hide-timestamps nil)


  (unless (file-exists-p erc-log-channels-directory)
    (mkdir erc-log-channels-directory t))

  (require 'erc-fill)
  (erc-fill-mode t)

  (require 'erc-autoaway)
  (setq erc-autoaway-idle-seconds 1200)
  (setq erc-autoaway-message "I'm gone (autoaway after %i seconds)")
  (setq erc-auto-discard-away t)

  (setq erc-auto-query 'buffer)

  (eval-after-load 'erc-track
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT")
        erc-track-switch-direction 'newest
        erc-track-enable-keybindings t))

  (require 'erc-services)
  (erc-nickserv-mode 1)
  (setq erc-prompt-for-nickserv-password nil)

  (require 'erc-menu)

)

(provide 'erc-conf)