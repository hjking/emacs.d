
;; http://www.emacswiki.org/emacs/TabBarMode
(when (require 'tabbar nil t)

  ;; From https://gist.github.com/3demax/1264635
  ;; Tabbar settings
  ; (set-face-attribute 'tabbar-default nil
  ;                     :background "gray20"
  ;                     :foreground "gray20"
  ;                     :box '(:line-width 1 :color "gray20" :style nil))
  ; (set-face-attribute 'tabbar-unselected nil
  ;                     :background "gray30"
  ;                     :foreground "white"
  ;                     :box '(:line-width 5 :color "gray30" :style nil))
  ; (set-face-attribute 'tabbar-selected nil
  ;                     :background "gray75"
  ;                     :foreground "black"
  ;                     :box '(:line-width 5 :color "gray75" :style nil))
  ; (set-face-attribute 'tabbar-highlight nil
  ;                     :background "white"
  ;                     :foreground "black"
  ;                     :underline nil
  ;                     :box '(:line-width 5 :color "white" :style nil))
  ; (set-face-attribute 'tabbar-button nil
  ;                     :box '(:line-width 1 :color "gray20" :style nil))
  ; (set-face-attribute 'tabbar-separator nil
  ;                     :background "gray20"
  ;                     :height 0.6)

  ;; From https://github.com/Ranler/dotfiles/blob/master/.emacs.d/tabbar-config.el
  ;;;; 设置tabbar外观
  ;; 设置默认主题: 字体, 背景和前景颜色，大小
  (set-face-attribute 'tabbar-default nil
                      :family "DejaVu Sans Mono"
                      :background "black"
                      :foreground "#48A4D5"
                      :height 0.9
                      )
  ;; 设置左边按钮外观：外框框边大小和颜色
  (set-face-attribute 'tabbar-button nil
                      :inherit 'tabbar-default
                      :box '(:line-width 2 :color "#48A4D5")
                      )
  ;; 设置当前tab外观：颜色，字体，外框大小和颜色
  (set-face-attribute 'tabbar-selected nil
                      :inherit 'tabbar-default
                      :foreground "red"
                      :background "#48A4D5"
                      :box '(:line-width 2 :color "#48A4D5" :style pressed-button)
                      ;; :overline "black"
                      ;; :underline "black"
                      :weight 'bold
                      )
  ;; 设置非当前tab外观：外框大小和颜色
  (set-face-attribute 'tabbar-unselected nil
                      :inherit 'tabbar-default
                      :box '(:line-width 2 :color "black" :style released-button)
                      )

  ;; Change padding of the tabs
  ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
  (setq tabbar-separator '(0.5))
  ;; adding spaces
  (defun tabbar-buffer-tab-label (tab)
    "Return a label for TAB.
  That is, a string used to represent it on the tab bar."
    (let ((label  (if tabbar--buffer-show-groups
                      (format "[%s]  " (tabbar-tab-tabset tab))
                    (format "%s  " (tabbar-tab-value tab)))))
      ;; Unless the tab bar auto scrolls to keep the selected tab
      ;; visible, shorten the tab label to keep as many tabs as possible
      ;; in the visible area of the tab bar.
      (if tabbar-auto-scroll-flag
          label
        (tabbar-shorten
         label (max 1 (/ (window-width)
                         (length (tabbar-view
                                  (tabbar-current-tabset)))))))))
  (tabbar-mode t))

(provide 'tabbar-conf)