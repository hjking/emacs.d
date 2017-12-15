
;; A Collection of Ridiculously Useful eXtensions for Emacs

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line))
  :config
    (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
    ; (global-set-key (kbd "C-c o") #'crux-open-with)
    (global-set-key [(shift return)] #'crux-smart-open-line)
    ; (global-set-key (kbd "s-r") #'crux-recentf-find-file)
    ; (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
    (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)

    ;; Using the bundled advices
    ;; You can use crux-with-region-or-buffer to make a command acting normally
    ;; on a region to operate on the entire buffer in the absence of a region.
    (crux-with-region-or-buffer indent-region)
    (crux-with-region-or-buffer untabify)

    ;; Likewise, you can use crux-with-region-or-line to make a command alternately
    ;; act on the current line if the mark is not active
    (crux-with-region-or-line comment-or-uncomment-region)
    (crux-with-region-or-point-to-eol)

    ;; Sometimes you might want to act on the point until the end of the current
    ;; line, rather than the whole line, in the absence of a region
    (crux-with-region-or-point-to-eol kill-ring-save)

  )

(provide 'crux-conf)