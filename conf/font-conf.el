
(message "%d: >>>>> Loading [ Font ] Customization ...." step_no)
(setq step_no (1+ step_no))

;; From https://github.com/lunaryorn/stante-pede/blob/master/init.el

(defconst stante-preferred-monospace-fonts
  `(("Source Code Pro" . ,(if (eq system-type 'darwin) 110 90))
    ("WenQuanYi Zen Hei" . 100)
    ("Inconsolata" . ,(if (eq system-type 'darwin) 120 100))
    ("Consolas" . 100)
    ("DejaVu Sans Mono" 110)
    ("Anonymous Pro" . ,(if (eq system-type 'darwin) 120 100))
    ("Anonymous Pro Minus" . ,(if (eq system-type 'darwin) 120 100))
    ("Courier New" . 100))
  "Preferred monospace fonts for Stante.

The `car' of each item is the font family, the `cdr' the preferred font size.")

(defconst stante-preferred-proportional-fonts
  '(("Lucida Grande" . 120)
    ("DejaVu Sans" . 110))
  "Preferred proportional fonts for Stante.

The `car' of each item is the font family, the `cdr' the preferred font size.")

(defun stante-first-existing-font (fonts)
  "Get the first existing font from FONTS."
  (--first (x-family-fonts (car it)) fonts))

(defun stante-choose-best-fonts ()
  "Choose the best fonts."
  (interactive)
  (-when-let (font  (stante-first-existing-font stante-preferred-monospace-fonts))
    (--each '(default fixed-pitch)
      (set-face-attribute it nil
                          :family (car font) :height (cdr font))))
  (-when-let (font (stante-first-existing-font stante-preferred-proportional-fonts))
    (set-face-attribute 'variable-pitch nil
                          :family (car font) :height (cdr font))))

(stante-choose-best-fonts)

; (ignore-errors
;   (cond ((eq system-type 'gnu/linux)
;          (set-frame-font "DejaVu Sans Mono"))
;         ((eq system-type 'windows-nt)
;          (set-frame-font "Lucida Sans Typewriter"))))
; (set-face-attribute 'default nil :height 113)

(provide 'font-conf)