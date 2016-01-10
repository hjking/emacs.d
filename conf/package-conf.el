
(message "%d: >>>>> Loading [ Package Manager ] Customization ...." step_no)
(setq step_no (1+ step_no))
(require 'package)

(dolist (source '(
                  ; ("marmalade" . "https://marmalade-repo.org/packages/")
                  ; ("elpa" . "http://tromey.com/elpa/")
                  ;; stable melpa
                  ; ("melpa-stable" . "http://stable.melpa.org/packages/")
                  ;; (development versions of packages)
                  ("melpa" . "http://melpa.org/packages/")
                  ; ("myelpa" . "~/.emacs.d/myelpa")
                  ; ("myelpa" . "https://raw.githubusercontent.com/redguardtoo/myelpa/master/")
                  ;; ("org" . "http://orgmode.org/elpa/")
                  ))
(add-to-list 'package-archives source t))

(when (< emacs-major-version 24)
    (add-to-list 'package-archives
                 '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defvar my-packages
  '(use-package
    ace-window
    aggressive-indent
    ; auto-complete-verilog
    company-c-headers
    company
    diminish
    dired+
    dired-details
    dired-details+
    dired-sort-menu
    dired-sort-menu+
    ecb
    ; evil
    ; evil-nerd-commenter
    expand-region
    ; ggtags
    gnuplot-mode
    guide-key
    guide-key-tip
    helm
    helm-projectile
    indent-guide
    info+
    miniedit
    org-toc
    org-trello
    paredit
    ; plantuml-mode
    perspective
    persp-projectile
    projectile
    ; smartparens
    ; smex
    smooth-scrolling
    stripe-buffer               ;; different background for even and odd lines
    ; web-mode
    ztree
  )
"A list of packages to ensure are installed at launch.")

;;    ;; Auto install the required packages
;;    ;; package-installed-p is from package.el and checks if a package is installed
;;    ;; Source: http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
;;    ;; method to check if all packages are installed
;;    (defun packages-installed-p ()
;;      (loop for p in my-packages
;;            when (not (package-installed-p p)) do (return nil)
;;            finally (return t)))
;;
;;    ;; if not all packages are installed, check one by one and install the missing ones.
;;    (unless (packages-installed-p)
;;      ; check for new packages (package versions)
;;      (message "%s" "Emacs is now refreshing its package database...")
;;      (package-refresh-contents)
;;      (message "%s" " done.")
;;      ; install the missing packages
;;      (dolist (p my-packages)
;;        (when (not (package-installed-p p))
;;          (package-install p))))
;;
;;    (defun package-require (pkg)
;;        "Install a package only if it's not already installed."
;;        (when (not (package-installed-p pkg))
;;        (package-install pkg)))

(defun prelude-packages-installed-p ()
  "Check if all packages in `my-packages' are installed."
  (every #'package-installed-p my-packages))

(defun prelude-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package my-packages)
    (add-to-list 'my-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun prelude-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'prelude-require-package packages))

(define-obsolete-function-alias 'prelude-ensure-module-deps 'prelude-require-packages)

(defun prelude-install-packages ()
  "Install all packages listed in `my-packages'."
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (prelude-require-packages my-packages)))

;; run package installation
; (prelude-install-packages)

(provide 'package-conf)
