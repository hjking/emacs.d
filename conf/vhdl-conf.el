;; -*- lexical-binding: t -*-
;;

(require 'vhdl-mode)
(autoload 'vhdl-mode "vhdl-mode" "VHDL Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.vhd?\\'" . vhdl-mode))
(vhdl-set-style "IEEE")

(provide 'vhdl-conf)