
(when (>= emacs-major-version 24)
  (message "%d: >>>>> Loading [ Package Manager ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  (require 'package)

  ; (add-to-list 'package-archives
  ;              '("marmalade" . "http://marmalade-repo.org/packages/")
  ;              t)

  ; ;; under active development.
  ; (add-to-list 'package-archives
  ;              '("melpa" . "http://melpa.milkbox.net/packages/")
  ;              t)
  ;; stable
  ; (add-to-list 'package-archives
  ;              '("melpa-stable" . "http://hiddencameras.milkbox.net/packages/")
  ;              t)

  (dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                    ("elpa" . "http://tromey.com/elpa/")
                    ;; stable
                    ; ("melpa-stable" . "http://hiddencameras.milkbox.net/packages/")
                    ;; TODO: Maybe, use this after emacs24 is released
                    ;; (development versions of packages)
                    ("melpa" . "http://melpa.milkbox.net/packages/")
                    ))
  (add-to-list 'package-archives source t))

  (when (< emacs-major-version 24)
      (add-to-list 'package-archives
                   '("gnu" . "http://elpa.gnu.org/packages/")))
  (package-initialize)

  (defvar my-packages
  '(
    auto-complete
    auto-complete-c-headers
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
    ; auto-complete-verilog
    )
  "A list of packages to ensure are installed at launch.")

  ;; Auto install the required packages
  ;; Source: https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el
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
