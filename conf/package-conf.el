
(message "%d: >>>>> Loading [ Package Manager ] Customization ...." step_no)
(setq step_no (1+ step_no))
(require 'package)

(dolist (source '(
                  ;; marmalade
                  ; ("marmalade" . "http://marmalade-repo.org/packages/")
                  ;; tromey
                  ; ("elpa" . "http://tromey.com/elpa/")
                  ;; stable melpa
                  ; ("melpa-stable" . "http://stable.melpa.org/packages/")
                  ;; development versions of melpa packages
                  ; ("melpa" . "http://melpa.org/packages/")

                  ;; emacs-china mirror
                  ; ("melpa" . "http://elpa.zilongshanren.com/melpa/")
                  ; ("gnu"   . "http://elpa.zilongshanren.com/gnu/")

                  ;; tsinghua mirror
                  ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                  ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                  ; ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                  ; ("myelpa" . "~/.emacs.d/myelpa")
                  ; ("myelpa" . "https://raw.githubusercontent.com/redguardtoo/myelpa/master/")
                  ;; org
                  ; ("org" . "http://orgmode.org/elpa/")
                  ))
(add-to-list 'package-archives source t))

; (when (< emacs-major-version 24)
;     (add-to-list 'package-archives
;                  '("gnu" . "http://elpa.gnu.org/packages/")))

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
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
    ; guide-key
    ; guide-key-tip
    ; helm
    ; helm-projectile
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
    ranger                  ;; Bringing the goodness of ranger to dired
    counsel
    swiper
    ; smartparens
    ; smex
    smooth-scrolling
    stripe-buffer               ;; different background for even and odd lines
    ; web-mode
    ztree
    beacon
    paradox
    evil-nerd-commenter
    calfw
    general
    origami
    color-theme-sanityinc-tomorrow
    deft
    wttrin
    crux
    ivy-hydra
    spaceline
    goto-last-change
    neotree
    gotham-theme
    monokai-theme
    popup
    esup
    discover-my-major
    csv-mode
    graphviz-dot-mode
  )
"A list of packages to ensure are installed at launch.")

;; From prelude
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
  "Ensure PACKAGES are installed. Missing packages are installed automatically."
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

;; run package installation, install packages in `my-packages automatically
; (prelude-install-packages)

(provide 'package-conf)
