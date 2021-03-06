;; -*- lexical-binding: t -*-
;;
;; Filename: dired-conf.el
;; Description: Setting for dired.el, dired-tar.el, dired-single, dired-x.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2014-01-24 10:21:00
;;

(use-package dired
  :ensure nil
  :config
  (progn
    (put 'dired-find-alternate-file 'disabled nil)
    ;; Dired copy folders recursively without confirmation
    (setq dired-recursive-copies 'always)
    ;; Dired delete folders recursively after confirmation
    (setq dired-recursive-deletes 'top)
    ;; setting for view CVS
    (setq cvs-dired-use-hook 'always)
    ;; try to guess a default target directory
    (setq dired-dwim-target t)
    ;; do what i mean when copy file with two windows
    (setq dired-isearch-filenames t)

    ;; Sort Directories First
    ;; if it is not Windows, use the following listing switches
    (when (not (eq system-type 'windows-nt))
      (setq dired-listing-switches "-lha --group-directories-first"))

    (defun hjking-dired-mode-hook ()
      "to be run as hook for `dired-mode'."
      (dired-hide-details-mode 1)   ;; hide the file's unix owner and permission info
      (visual-line-mode 0) ;; unwrap lines.
      )
    (add-hook 'dired-mode-hook 'hjking-dired-mode-hook)

    (add-hook 'dired-mode-hook
        '(lambda()
           ;; use the same buffer for viewing directory
           (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
           ;; open dir/file in other window
           ; (define-key dired-mode-map [return] 'dired-find-file-other-window)
           (define-key dired-mode-map [delete] 'dired-flag-file-deletion)
           (define-key dired-mode-map [C-down-mouse-1] 'dired-mouse-find-file-other-window)
           (define-key dired-mode-map [mouse-2] 'dired-find-file)
           (define-key dired-mode-map [mouse-3] 'dired-maybe-insert-subdir)
           (define-key dired-mode-map (kbd "C-{") 'dired-narrow-window)))
))

;; Load Dired X when Dired is loaded.
;; call `dired-jump [Ctrl+x Ctrl+j] to jump to the directory of current buffer
(use-package dired-x
  :after dired
  :config
  ;; Omit mode. hide uninteresting files, such as backup files
  (setq dired-omit-mode t) ; Turn on Omit mode.
  (setq-default dired-omit-files-p t) ; this is buffer-local variable
  (setq dired-omit-extensions
  '(".svn/" "CVS/" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico"
    ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd"
    ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm"
    ".class" ".lib" ".mem" ".x86f" ".sparcf" ".fasl"
    ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".lo" ".la" ".gmo"
    ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr"
    ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"
    ".DS_STORE" ".pdb" ".ilk"
   ))
  ;; hide my dot-files when hit M-o
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-omit-size-limit 1000000)
  )

;;; dired-details
;;   ) - dired-details-show
;;   ( - dired-details-hide
(use-package dired-details
  :after dired
  :init
  ;; show sym link targets
  (setq dired-details-hide-link-targets nil)
  :config
  (progn
    (dired-details-install)
    ;; both `)' and `(' to `dired-details-toggle'
    (use-package dired-details+)))

;; ls-lisp+
; (use-package ls-lisp+)
; (use-package files+)

;; sort
(use-package dired-sort
  :after dired)

;; dired-sort-menu
(use-package dired-sort-menu+
  :after dired
  :config
  (progn
    (add-hook 'dired-load-hook (lambda () (require 'dired-sort-menu)))))

;;; dired+
;; F to open all marked files. (nice!)
(use-package dired+
  :after dired
  :defer t)

;;; dired-view
(use-package dired-view
  :after dired
  :defer t
  :config
  (progn
    (add-hook 'dired-mode-hook 'dired-view-minor-mode-on)
    ;; define keys to toggle it
    (define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
    (define-key dired-mode-map (kbd ":") 'dired-view-minor-mode-dired-toggle)))

;;; dired-isearch
(use-package dired-isearch
  :after dired
  :config
  (progn
    (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
    (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
    (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
    (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)))

;;; dired-hacks
; (use-package dired-rainbow)
; (use-package dired-open)
; (use-package dired-subtree
;   :after dired
;   :config
;     (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
;     (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;;; ztree
;; ztree-diff: Perform diff on two directories
;; ztree-dir: a simple tree explorer
(use-package ztree-diff
  :commands (ztree-diff))

(use-package ztree-dir
  :commands (ztree-dir))

; (use-package dired-single)

;;; DiredTar
;;;
;;------- "T" compress dir to .tar.gz file
(use-package dired-tar
  :after dired
  )

;;narrow dired to match filter
(use-package dired-narrow
  :bind (:map dired-mode-map
        ("/" . dired-narrow)))


(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))


(use-package dired-ranger
  :bind (:map dired-mode-map
        ("W" . dired-ranger-copy)
        ("X" . dired-ranger-move)
        ("Y" . dired-ranger-paste)))

(provide 'dired-conf)