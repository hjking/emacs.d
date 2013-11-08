
(message "%d: >>>>> Loading [ VHDL Mode ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(autoload 'vhdl-mode "vhdl-mode" "VHDL Mode" t)
(setq auto-mode-alist (cons '("\\.vhdl?\\'" . vhdl-mode) auto-mode-alist))
