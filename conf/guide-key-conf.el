
(require 'guide-key)

;; (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(setq guide-key/guide-key-sequence '("C-x r" ;; rectangle, registers
                                     "C-x 4" ;; commands that operate in other window (buffer)
                                     "C-x 5" ;; commands that operate in other frame (window)
                                     "C-x"
                                     "C-c"
                                     (outline-minor-mode "C-c @")))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)  ; Enable guide-key-mode

;; highlight only rectangle family commands when press "C-x r"
;; (setq guide-key/highlight-command-regexp "rectangle")
;; highlight both rectangle family and register family when press "C-x r"
(setq guide-key/highlight-command-regexp "rectangle\\|register")

;; for org mode
(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

(diminish 'guide-key-mode)

(require 'guide-key-tip)
(setq guide-key-tip/enabled t)