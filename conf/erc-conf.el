
;; Filename: erc-conf.el
;; Description: Setting for erc.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2012-04-19 17:23:16
;;

;; ERC, an Emacs IRC client
(use-package erc
  :disabled t
  :defer 10
  ; :commands (erc-select-read-args erc-select erc-select-ssl)
  :init (progn
    (setq erc-kill-buffer-on-part            t
          erc-server-auto-reconnect          t
          erc-server-coding-system           '(utf-8 . utf-8)
          erc-kill-queries-on-quit           t
          erc-default-coding-system          '(utf-8 . utf-8)
          erc-hide-list                      '("JOIN" "PART" "QUIT" "NICK" "MODE")
          erc-kill-server-buffer-on-quit     t
          erc-server                         "irc.freenode.net"
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
          erc-ignore-list                    '("jibot")  ; nil
          erc-keywords                       '("emms" "python" "emacs" "org" "vim")
          )

    ;; set coding seperatly for some channel
    (setq erc-encoding-coding-alist '(("#linuxfire" . chinese-iso-8bit)))
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
          '(("freenode.net" "#org-mode" "#emacs" "#hacklabto" "#vim" "#git")
            ; ("oftc.net" "#debian-zh" "#emacs-cn")
            ; ("irc.int.ru" "#unix")
            ; ("irc.fu-berlin.de" "#unix.ru" "#fidorus")
            ; ("fu-berlin.de" "#unix.ru" "#fidorus")
            ))

    (setq erc-part-reason-various-alist erc-quit-reason-various-alist
          erc-part-reason               'erc-part-reason-various
          erc-quit-reason               'erc-quit-reason-various)

    (defun hjking-freenode ()
      (interactive (list (y-or-n-p "Autojoin channels? ")))
      (erc :server "irc.freenode.net"
           :port 6667
           :nick "hjking"))

    (defun hjking-gitter-irc ()
      (interactive)
      (erc :server "irc.gitter.im"
           :port 6667
           :nick "hjking"))

    ; auto join
    ; (erc :server "irc.freenode.net" :port 6667 :nick "hjking")

    )
  :config (progn
    ;; highlight keywords or users which i'm interested
    (erc-match-mode 1)
    ;; timestamp
    (erc-timestamp-mode 1)

    (use-package erc-log
      :init (progn
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
        )
      :config (progn
        (erc-log-mode 1)
        )
      )

    (use-package erc-fill
      :config (erc-fill-mode t)
      )

    (use-package erc-autoaway
      :init (progn
        (setq erc-autoaway-idle-seconds 1200)
        (setq erc-autoaway-message "I'm gone (autoaway after %i seconds)")
        (setq erc-auto-discard-away t)
        (setq erc-auto-query 'buffer))
      :config (erc-fill-mode t)
      )

    (use-package erc-track
      :init (progn
        (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT")
              erc-track-switch-direction 'newest
              erc-track-enable-keybindings t)
        )
      )

    (use-package erc-services
      :init (setq erc-prompt-for-nickserv-password nil)
      :config (erc-nickserv-mode 1)
      )

    (use-package erc-menu)

    )
)

(provide 'erc-conf)