
(message "%d: >>>>> Loading [ mmm-mode ] Customization ...." step_no)
(setq step_no (1+ step_no))

(require 'mmm-auto)

;; load itself whenever you open an appropriate file
(setq mmm-global-mode 'maybe)
;; notice PHP regions in html-mode files having a `.php' extension
(mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
(autoload 'mmm-mode "mmm-mode" "Multiple Major Modes" t)
(autoload 'mmm-parse-buffer "mmm-mode" "Automatic MMM-ification" t)
