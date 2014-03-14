


(when (>= emacs-major-version 24)
  (message "%d: >>>>> Loading [ Package Manager ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  (require 'package)
  ; (setq package-archives
  ;   '(
  ;     ("gnu"         . "http://elpa.gnu.org/packages/")
  ;     ("marmalade"   . "http://marmalade-repo.org/packages/")
  ;     ("melpa"       . "http://melpa.milkbox.net/packages/")
  ;     ("elpa"        . "http://tromey.com/elpa/")
  ;     ("josh"        . "http://josh.github.com/elpa/")
  ;     ("technomancy" . "http://repo.technomancy.us/emacs/")))
  ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
           '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
           '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (when (< emacs-major-version 24)
      (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
  (package-initialize)


  (defun package-require (pkg)
      "Install a package only if it's not already installed."
      (when (not (package-installed-p pkg))
      (package-install pkg)))
)
