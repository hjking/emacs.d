
;; Filename: dired-conf.el
;; Description: Setting for dired.el, dired-tar.el, dired-single, dired-x.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2014-01-24 10:21:00
;;
(message "%d: >>>>> Loading [ Dired ] Customization ...." step_no)
(setq step_no (1+ step_no))

;;; dired-details
;;;
;;   ) - dired-details-show
;;   ( - dired-details-hide
(message "    >>>>> Loading [ dired-details ] Customization ....")
;; (require 'dired-details)
;; (dired-details-install)
;; (setq dired-details-hidden-string "")
;; dired-details+
(require 'dired-details+) ;; auto load `dired-details'

;;; ls-lisp
;;;
;; emulate insert-directory completely in Emacs Lisp
(message "    >>>>> Loading [ ls-lisp ] Customization ....")
(when (require 'ls-lisp nil t)
    ;; disable the case sensitive sort of file names
    (setq ls-lisp-ignore-case t)
    ;; sort directories first in any ordering
    (setq ls-lisp-dirs-first t)
    ;; use ISO 8601 dates (on MS-Windows)
    (setq ls-lisp-format-time-list
           '("%Y-%m-%d %H:%M"
             "%Y-%m-%d %H:%M"))
    ;; use localized date/time format
    (setq ls-lisp-use-localized-time-format t)
)

;; ls-lisp+
(require 'ls-lisp+)
(require 'files+)


;;; dired setting
;; Dired copy folders recursively without confirmation
(setq dired-recursive-copies 'always)
;; Dired delete folders recursively after confirmation
(setq dired-recursive-deletes 'top)
;; setting for view CVS
(setq cvs-dired-use-hook 'always)
;; try to guess a default target directory
(setq dired-dwim-target t)
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

(message "    >>>>> Loading [ dired-sort ] Customization ....")
;; sort
(require 'dired-sort)

;; dired-sort-menu
(add-hook 'dired-load-hook (lambda () (require 'dired-sort-menu)))
(require 'dired-sort-menu+)

;; ‘C-x C-q’ runs the command ‘wdired-change-to-wdired-mode’ to make a dired buffer editable.
;; Commit changes to disk with ‘C-c C-c’


;;; DiredTar
;;;
;;------- "T" compress dir to .tar.gz file
(require 'dired-tar)
;; no line wrap
(defun my-dired-long-lines ()
  (setq truncate-lines t))
(add-hook 'dired-after-readin-hook 'my-dired-long-lines)
;; C-x C-j open the directory of current buffer
(global-set-key (kbd "C-x C-j")
  (lambda ()
    (interactive)
    (if (buffer-file-name) (dired default-directory))
  )
)
;;-------------------------------------------------------------------------


;;; dired-x setting
;;;
(message "    >>>>> Loading [ dired-x ] Customization ....")
(require 'dired-x nil t)
;; Load Dired X when Dired is loaded.
(add-hook 'dired-load-hook
          (lambda ()
                  (load "dired-x")
                  ;; Set global variables here.  For example:
                  ;; (setq dired-guess-shell-gnutar "gtar")
                  ))
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
                  (define-key dired-mode-map (kbd "C-o") 'dired-omit-mode)
          ))
;;-------------------------------------------------------------------------


;;; dired-single setting
(message "    >>>>> Loading [ dired-single ] Customization ....")
(require 'dired-single)
;;-------------------------------------------------------------------------


;;; dired+
;;;
;;; "F" open all marked files
;;; "* ." mark files by extensions
(message "    >>>>> Loading [ dired+ ] Customization ....")
(require 'dired+)


;;; dired-view
;;;
;; Browse and select files using the first character of their names
(message "    >>>>> Loading [ dired-view ] Customization ....")
(require 'dired-view)
(add-hook 'dired-mode-hook 'dired-view-minor-mode-on)
;; define keys to toggle it
(define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
(define-key dired-mode-map (kbd ":") 'dired-view-minor-mode-dired-toggle)

;;; dired-isearch
(require 'dired-isearch)
(define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
(define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
(define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
(define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)

;;; dired-hacks
(require 'dired-rainbow)
(require 'dired-open)


;;; ztree
;; ztree-diff: Perform diff on two directories
;; ztree-dir: a simple tree explorer
(require 'ztree-diff)
(require 'ztree-dir)