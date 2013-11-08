
;; An emacs mode for git
(message "%d: >>>>> Loading [ git ] Customizations ...." step_no)
(setq step_no (1+ step_no))

;;
;; git-modes
;;
(require 'git-commit-mode)
(require 'git-rebase-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)

;;
;; magit
;;
(require 'magit)
(require 'magit-svn)
(autoload 'magit-status "magit" nil t)
(setq magit-process-connection-type nil)
(setq magit-repo-dirs-depth 2)
(setq magit-save-some-buffers nil)

;; Add an extra newline to separate commit message from git commentary
(defun magit-commit-mode-init ()
  (when (looking-at "\n")
    (open-line 1)))

(add-hook 'git-commit-mode-hook 'magit-commit-mode-init)

;; close popup when commiting
(defadvice git-commit-commit (after delete-window activate)
  (delete-window))

(add-hook 'magit-log-edit-mode-hook
 (lambda ()
   (setq fill-paragraph-function nil)))

(global-set-key (kbd "M-g s") 'magit-status)
(global-set-key (kbd "M-g c") 'magit-cheat-sheet)
(defun magit-cheat-sheet ()
  (interactive)
  (browse-url "http://daemianmack.com/magit-cheatsheet.html"))
(global-set-key (kbd "C-x g") 'magit-status)

;;
;; git-emacs
;;
; (message "%d: >>>>> Loading [ git-emacs ] Customizations ...." step_no)
; (setq step_no (1+ step_no))

; (require 'git-emacs)
