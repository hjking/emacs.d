(setq auto-mode-alist
      (cons '("\\.gp$" . gnuplot-mode) auto-mode-alist))

(autoload 'gnuplot-mode "gnuplot-mode" "Gnuplot mode" t)