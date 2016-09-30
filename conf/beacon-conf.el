;; Time-stamp: <2015-11-10 16:09:22 hjking>

;; Beacon
;; https://github.com/Malabarba/beacon

(use-package beacon
  :init
    (setq beacon-blink-when-point-moves-vertically nil) ; default nil
    (setq beacon-blink-when-point-moves-horizontally nil) ; default nil
    (setq beacon-blink-when-buffer-changes t) ; default t
    (setq beacon-blink-when-window-scrolls t) ; default t
    (setq beacon-blink-when-window-changes t) ; default t
    (setq beacon-blink-when-focused nil) ; default nil

    (setq beacon-blink-duration 0.6) ; default 0.3
    (setq beacon-blink-delay 0.3) ; default 0.3
    (setq beacon-size 40) ; default 40
    ;; (setq beacon-color "yellow") ; default 0.5
    (setq beacon-color 0.5) ; default 0.5
  :config
  (progn
    (add-to-list 'beacon-dont-blink-major-modes 'term-mode)

    (beacon-mode 1)))


(provide 'beacon-conf)
