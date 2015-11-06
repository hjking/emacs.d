
;; Filename: dired-conf.el
;; Description: Setting for dired.el, dired-tar.el, dired-single, dired-x.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2014-01-24 10:21:00
;;
(message "%d: >>>>> Loading [ Dired ] Customization ...." step_no)
(setq step_no (1+ step_no))

(use-package dired
  :config
  (progn
    ;; Dired copy folders recursively without confirmation
    (setq dired-recursive-copies 'always)
    ;; Dired delete folders recursively after confirmation
    (setq dired-recursive-deletes 'top)
    ;; setting for view CVS
    (setq cvs-dired-use-hook 'always)
    ;; try to guess a default target directory
    (setq dired-dwim-target t)
    (setq dired-isearch-filenames t)
    ;; enable the use of the command dired-find-alternate-file without confirmation
    (put 'dired-find-alternate-file 'disabled nil)
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    ;; Sort Directories First
    ;; if it is not Windows, use the following listing switches
    (when (not (eq system-type 'windows-nt))
      (setq dired-listing-switches "-lha --group-directories-first"))
    ;; show sym link targets
    (setq dired-details-hide-link-targets nil)

    (add-hook 'dired-mode-hook
        '(lambda()
           (visual-line-mode 0) ;; unwrap lines.
           (define-key dired-mode-map [delete] 'dired-flag-file-deletion)
           (define-key dired-mode-map [return] 'dired-find-file-other-window)
           (define-key dired-mode-map [C-down-mouse-1] 'dired-mouse-find-file-other-window)
           (define-key dired-mode-map [mouse-2] 'dired-find-file)
           (define-key dired-mode-map [mouse-3] 'dired-maybe-insert-subdir)
           (define-key dired-mode-map (kbd "C-{") 'dired-narrow-window)))

    ;;  DiredOmit Mode
    (add-hook 'dired-mode-hook
          (lambda ()
                  ;; Set buffer-local variables here.  For example:
                  (dired-omit-mode 1)
                  (setq-default dired-omit-files-p t) ; this is buffer-local variable
                  (setq dired-omit-extensions
                        '(".svn/" "CVS/" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico"
                          ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd"
                          ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm"
                          ".class" ".lib" ".mem" ".x86f" ".sparcf" ".fasl"
                          ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".lo" ".la" ".gmo"
                          ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr"
                          ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"
                          ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cps" ".fn"
                          ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs"
                          ".pdb" ".ilk"
                         ))
                  ;; hide my dot-files when hit M-o
                  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
                  (setq dired-omit-size-limit 1000000)
                  (define-key dired-mode-map (kbd "C-o") 'dired-omit-mode)))

    (when (eq system-type 'darwin)
      (add-to-list 'dired-omit-extensions ".DS_STORE"))

    ;; Load Dired X when Dired is loaded.
    (use-package dired-x)

    ;;; dired-details
    ;;   ) - dired-details-show
    ;;   ( - dired-details-hide
    (use-package dired-details
      :config
      (progn
        (dired-details-install)
        (use-package dired-details+)))

    ;; ls-lisp+
    (use-package ls-lisp+)
    (use-package files+)

    ;; sort
    (use-package dired-sort)

    ;; dired-sort-menu
    (add-hook 'dired-load-hook (lambda () (require 'dired-sort-menu)))
    (use-package dired-sort-menu+)

    ;;; dired+
    (use-package dired+
      :defer t)


    ;;; dired-view
    (use-package dired-view
      :defer t
      :config
      (progn
        (add-hook 'dired-mode-hook 'dired-view-minor-mode-on)
        ;; define keys to toggle it
        (define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
        (define-key dired-mode-map (kbd ":") 'dired-view-minor-mode-dired-toggle)))

    ;;; dired-isearch
    (use-package dired-isearch
      :config
      (progn
        (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
        (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
        (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
        (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)))

    ;;; dired-hacks
    (use-package dired-rainbow)
    (use-package dired-open)

    ;;; ztree
    ;; ztree-diff: Perform diff on two directories
    ;; ztree-dir: a simple tree explorer
    (use-package ztree-diff)
    (use-package ztree-dir)

    (use-package dired-single)

    ;;; DiredTar
    ;;;
    ;;------- "T" compress dir to .tar.gz file
    (use-package dired-tar)

))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dired-conf)