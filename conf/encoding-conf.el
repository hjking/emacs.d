
(message "%d: >>>>> Loading [ Coding ] Customizations ...." step_no)
(setq step_no (1+ step_no))
(set-language-environment 'Chinese-GB)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(euc-cn . euc-cn))
(setq default-buffer-file-coding-system 'utf-8)
(setq-default pathname-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)