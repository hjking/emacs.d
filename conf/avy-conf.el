;;
;; Filename: avy-conf.el
;; Description: avy allows us to effectively navigate to visible things by
;;              searching for a letter on the screen and jumping to it.
;; Author: Hong Jin
;; Last Updated: 2015-07-23 10:00
;;

;; https://github.com/abo-abo/avy

(use-package avy
  :commands (avy-goto-word-or-subword-1
             avy-goto-word-0
             avy-goto-word-1
             avy-goto-char
             avy-goto-char-2
             avy-goto-line)
  :bind (("M-s"   . avy-goto-word-1)
         ("M-g f" . avy-goto-line))
  :init (progn
         (setq avy-background t)
         (setq avy-style 'at-full)
         (setq avy-all-windows nil)))

(provide 'avy-conf)