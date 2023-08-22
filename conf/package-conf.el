;; -*- lexical-binding: t -*-
;;

(require 'package)

(setq package-archives '(
                         ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ; ("marmalade" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ;; emacs-china
                         ; ("gnu" . "http://elpa.emacs-china.org/gnu/")
                         ; ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ;; 163
                         ; ("gnu" . "https://mirrors.163.com/elpa/gnu/")
                         ; ("melpa" . "https://mirrors.163.com/elpa/melpa/")
                         ))

;; org
; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(when (< emacs-major-version 27)
  (package-initialize))

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
; (when (not package-archive-contents)
;   (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(use-package
    ; adaptive-wrap ; indented line wrapping
    ; ag wgrep wgrep-ag s ; ag > ack > grep
    ; auto-complete-verilog
    ; evil
    ; evil-nerd-commenter
    ; ggtags
    ; guide-key
    ; guide-key-tip
    ; helm
    ; helm-projectile
    ; highlight-global
    ; mode-line-stats
    plantuml-mode
    ; powerline
    ; smart-mode-line
    ; smartparens
    ; smex
    ; web-mode
    dash
    ace-window
    aggressive-indent
    async
    beacon ; visual flash to show the cursor position
    browse-kill-ring+
    buffer-move
    calfw
    clippy
    color-theme-sanityinc-tomorrow
    color-theme-solarized
    company
    company-c-headers
    counsel
    crux
    csv-mode
    deft ; quick note taking and management
    diminish
    dired+
    dired-details
    dired-details+
    dired-sort-menu
    dired-sort-menu+
    dired-subtree
    dired-hacks-utils
    discover-my-major
    dracula-theme
    drag-stuff
    ecb
    esup
    evil-nerd-commenter
    expand-region
    f
    faces+
    find-file-in-project
    fiplr
    flx-ido
    general
    gnuplot-mode
    google-c-style
    gotham-theme
    graphviz-dot-mode
    header2
    helpful
    hideshowvis
    highlight-symbol
    hl-anything
    ido-hacks
    ido-ubiquitous
    ido-vertical-mode
    ido-at-point
    indent-guide
    info+
    ivy-hydra
    key-chord
    magit
    markdown-mode
    miniedit
    mmm-mode anzu
    molokai-theme
    monokai-theme
    mpg123
    multi-term
    multiple-cursors
    names
    org-bullets
    org-toc
    org-trello
    origami
    paradox ; package menu improvements
    paredit
    persp-projectile
    perspective
    popup
    popwin
    pretty-mode
    projectile ; Better than fiplr
    rainbow-delimiters
    ranger ; Bringing the goodness of ranger to dired
    session
    smart-compile
    smart-hungry-delete
    smooth-scrolling
    spaceline
    stripe-buffer ; different background for even and odd lines
    sublimity
    swiper
    tabbar
    treemacs
    undo-tree
    use-package
    verilog-mode
    visual-regexp
    visual-regexp-steroids
    volatile-highlights
    w3m
    which-key
    wttrin ; weather
    zenburn-theme
    ztree
    neotree
    darktooth-theme
    major-mode-hydra
    anaconda-mode
    company-anaconda
    ivy-rich
    solarized-theme
    org-kanban
  )
"A list of packages to ensure are installed at launch.")


;; avoid write `package-selected-packages' to custom file
; (defun package--save-selected-packages (&rest opt) nil)

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
(prelude-install-packages)

(provide 'package-conf)
