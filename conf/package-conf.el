
(when (>= emacs-major-version 24)
  (message "%d: >>>>> Loading [ Package Manager ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  (require 'package)

  (dolist (source '(
                    ; ("marmalade" . "https://marmalade-repo.org/packages/")
                    ("elpa" . "http://tromey.com/elpa/")
                    ;; stable melpa
                    ; ("melpa-stable" . "http://stable.melpa.org/packages/")
                    ;; (development versions of packages)
                    ("melpa" . "http://melpa.org/packages/")
                    ;; ("org" . "http://orgmode.org/elpa/")
                    ))
  (add-to-list 'package-archives source t))

  (when (< emacs-major-version 24)
      (add-to-list 'package-archives
                   '("gnu" . "http://elpa.gnu.org/packages/")))
  (package-initialize)

  (defvar my-packages
  '(
    use-package
    company
    stripe-buffer               ;; different background for even and odd lines
    expand-region
    smex
    projectile
    info+
    indent-guide
    dired+
    dired-details
    dired-details+
    dired-sort-menu
    dired-sort-menu+
    smartparens
    perspective
    persp-projectile
    zencoding-mode
    org-toc
    smooth-scrolling
    web-mode
    helm
    helm-projectile
    ggtags
    diminish
    ztree
    gnuplot-mode
    ecb
    company-c-headers
    guide-key
    guide-key-tip
    ; evil
    ; evil-nerd-commenter
    ; auto-complete-verilog
    aggressive-indent
    miniedit
    )
  "A list of packages to ensure are installed at launch.")

  ;; Auto install the required packages
  ;; package-installed-p is from package.el and checks if a package is installed
  ;; Source: http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
  ;; method to check if all packages are installed
  (defun packages-installed-p ()
    (loop for p in my-packages
          when (not (package-installed-p p)) do (return nil)
          finally (return t)))

  ;; if not all packages are installed, check one by one and install the missing ones.
  (unless (packages-installed-p)
    ; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ; install the missing packages
    (dolist (p my-packages)
      (when (not (package-installed-p p))
        (package-install p))))

  (defun package-require (pkg)
      "Install a package only if it's not already installed."
      (when (not (package-installed-p pkg))
      (package-install pkg)))
)
