
(message "%d: >>>>> Loading [ htmlize ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(use-package htmlize
 :defer t
 :commands (htmlize-buffer)
 :config
 (progn
    (setq htmlize-html-major-mode 'html-mode)
    ;; output type of generated HTML
    (setq htmlize-output-type 'css)
    ;; charset declared by the resulting HTML documents
    (setq htmlize-html-charset "utf-8")
    ;; non-ASCII characters (codes in the 128-255 range) are copied to HTML
    ;; without modification -- if your HTML is in Unicode
    (setq htmlize-convert-nonascii-to-entities nil))
  :bind(
    ("M-P" . htmlize-buffer))
 )

(provide 'htmlize-conf)