;; Time-stamp: <2016-06-19 21:38:52 hjking>

;; evil-nerd-commenter
;; https://github.com/redguardtoo/evil-nerd-commenter

(use-package evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines
             evilnc-quick-comment-or-uncomment-to-the-line
             evilnc-copy-and-comment-lines
             evilnc-comment-or-uncomment-paragraphs)
  :bind (("M-;" . evilnc-comment-or-uncomment-lines))
  :config
    (evilnc-default-hotkeys))

(provide 'evil-nerd-commenter-conf)