
;; Filename: dired-conf.el
;; Description: Setting for dired.el, dired-tar.el, dired-single, dired-x.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2012-04-17 17:50:16
;;
(message "%d: >>>>> Loading [ Dired ] Customizations ...." step_no)
(setq step_no (1+ step_no))

;; emulate insert-directory completely in Emacs Lisp
(when (try-require 'ls-lisp)
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

(add-hook 'dired-mode-hook
    '(lambda()
       (define-key dired-mode-map [delete] 'dired-flag-file-deletion)
       (define-key dired-mode-map [return] 'dired-find-file-other-window)
       (define-key dired-mode-map [C-down-mouse-1] 'dired-mouse-find-file-other-window)
       (define-key dired-mode-map [mouse-2] 'dired-find-file)
    )
)
;; sort ( s s : sort by size ; s x : sort by extension; s t : sort by time; s b : sort by name )
(add-hook 'dired-mode-hook (lambda ()
  (interactive)
  (make-local-variable  'dired-sort-map)
  (setq dired-sort-map (make-sparse-keymap))
  (define-key dired-mode-map "s" dired-sort-map)
  (define-key dired-sort-map "s"
    '(lambda () "sort by Size"
       (interactive) (dired-sort-other (concat dired-listing-switches "S"))
    )
  )
  (define-key dired-sort-map "x"
    '(lambda () "sort by eXtension"
       (interactive) (dired-sort-other (concat dired-listing-switches "X"))
    )
  )
  (define-key dired-sort-map "t"
    '(lambda () "sort by Time"
       (interactive) (dired-sort-other (concat dired-listing-switches "t"))
    )
  )
  (define-key dired-sort-map "n"
    '(lambda () "sort by Name"
       (interactive) (dired-sort-other (concat dired-listing-switches ""))
      )
  ))
)
;; directory before files
(defun my-dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties) ;; not define dired-insert-set-properties
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil)
)
(add-hook 'dired-after-readin-hook 'my-dired-sort)
(add-hook 'dired-lood-hook 'my-dired-sort)

;; dir-isearch
(message ">>>>> Loading [ Dir-isearch ] Customizations ....")
(setq my-dir-isearch-dir (concat my-site-lisp-dir "dired-isearch/"))
(add-to-list 'load-path my-dir-isearch-dir)
(when (try-require 'dired-isearch)
   (require 'dired-isearch-autoloads)
)

;; *** --- wdired: rename filename
;; ‘C-x C-q’ runs the command ‘wdired-change-to-wdired-mode’ to make a dired buffer editable
(require 'wdired nil t)
(when (featurep 'wdired)
  (autoload 'wdired-change-to-wdired-mode "wdired")
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))


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
(message ">>>>> Loading [ dired-x ] Customizations ....")
(require 'dired-x nil t)
(setq-default dired-omit-files-p t)
(when (featurep 'dired-x)
  ;; (add-hook 'dired-load-hook
  ;;           (function (lambda () (load "dired-x"))))
  ;; (add-hook 'dired-mode-hook
  ;;           (function (lambda () (setq dired-omit-files-p t))))

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
  (setq dired-omit-extensions (delete ".pdf" dired-omit-extensions))
  ;; hide my dot-files when hit M-o
  ;; (setq dired-omit-files (concat "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.\\|^~"))
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-omit-size-limit 1000000)

  (add-to-list 'dired-guess-shell-alist-default '("\\.dvi$" "dvipdfmx"))
  (add-to-list 'dired-guess-shell-alist-default '("\\.pl$" "perltidy"))
)
;;-------------------------------------------------------------------------


;;; dired-single setting
(message ">>>>> Loading [ dired-single ] Customizations ....")
(require 'dired-single)
(add-hook 'dired-mode-hook
          (lambda ()
            ;; (define-key dired-mode-map [return]           'joc-dired-single-buffer)
            ;; (define-key dired-mode-map (kbd "<mouse-1>")  'joc-dired-single-buffer-mouse)
            ;; (define-key dired-mode-map "^"
            ;;   (function (lambda nil (interactive) (joc-dired-single-buffer ".."))))
            ;; (define-key dired-mode-map (kbd "C-x C-j")
            ;;   (function (lambda nil (interactive) (joc-dired-single-buffer ".."))))
            (setq dired-use-magic-buffer t)
            (setq dired-magic-buffer-name "*Dired*")))
(global-set-key (kbd "C-x d") 'joc-dired-single-magic-buffer)
;;-------------------------------------------------------------------------


;;; dired+
;;; "F" open all marked files
;;; "* ." mark files by extensions
;;;
(message ">>>>> Loading [ dired+ ] Customizations ....")
(require 'dired+)


;; dired-sort-menu
;;
(message ">>>>> Loading [ dired-sort-menu ] Customizations ....")
(require 'dired-sort-menu)
(add-hook 'dired-load-hook (lambda () (require 'dired-sort-menu)))


;; dired-view
;;
(message ">>>>> Loading [ dired-view ] Customizations ....")
(require 'dired-view)
(add-hook 'dired-mode-hook 'dired-view-minor-mode-on)
(define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
(define-key dired-mode-map (kbd ":") 'dired-view-minor-mode-dired-toggle)


;; dired-details
;;   ) - dired-details-show
;;   ( - dired-details-hide
(message ">>>>> Loading [ dired-details ] Customizations ....")
(require 'dired-details)
(dired-details-install)


;; dired-details+
(message ">>>>> Loading [ dired-details+ ] Customizations ....")
(require 'dired-details+)


;; diredful
;; color the filename
;; display files in different faces and colors
;; M-x diredful-add : define a new name for a new file type
;; M-x diredful-edit
;; M-x diredful-delete
;;
(message ">>>>> Loading [ diredful ] Customizations ....")
(require 'diredful)
;; (setq diredful-init-file "~/.emacs.d/diredful-conf.el")


;; ls-lisp+
(message ">>>>> Loading [ ls-lisp+ ] Customizations ....")
(require 'ls-lisp+)
