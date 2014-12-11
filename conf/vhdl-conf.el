
(message "%d: >>>>> Loading [ VHDL Mode ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(require 'vhdl-mode)
(autoload 'vhdl-mode "vhdl-mode" "VHDL Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.vhd?\\'" . vhdl-mode))
(vhdl-set-style "IEEE")

(provide 'vhdl-conf)