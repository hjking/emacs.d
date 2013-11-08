;;
;; Filename: tramp-conf.el
;; Description: Setting for tramp.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2012-11-21 15:25:20
;;
(message "%d: >>>>> Loading [ tramp ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-method-alist '("" "jinhong" "ssh"))
(add-to-list 'tramp-default-method-alist '("10.1.8.*" "jinhong" "smb"))
(add-to-list 'tramp-default-method-alist '("kinghom" "" "rsync"))
(add-to-list 'tramp-default-method-alist '("\\`localhost\\'" "\\`root\\'" "su"))
(add-to-list 'tramp-default-method-alist '("" "root" "ssh"))

(setq tramp-default-user "root")
(add-to-list 'tramp-default-user-alist '("ssh" ".*\\.somewhere\\.else\\'" "john"))
(add-to-list 'tramp-default-user-alist '("ssh" "\\`here\\.somewhere\\.else\\'" nil))

;; Backup dir
(setq tramp-backup-directory-alist backup-directory-alist)

(when win32p
  (require 'tramp "tramp" t)
  (setq tramp-password-end-of-line "\r\n")
  ;;(setq tramp-sh-program "d:/cygwin/bin/sh.exe")
  ;;(setq tramp-default-method "smx")
  ;; default transfer method
  (setq tramp-default-method  ; `scp' by default
        (cond (win32p
               ;; (issues with Cygwin `ssh' which does not cooperate with
               ;; Emacs processes -> use `plink' from PuTTY, it definitely
               ;; does work under Windows)
               ;; C-x C-f /plink:myuser@host:/some/directory/file
               "plink")
              (t
               "ssh")))
    ;; help debugging
  (setq tramp-verbose 10)
  (setq tramp-debug-buffer t)
  (setq tramp-auto-save-directory "c:/temp")
  ;; how many seconds passwords are cached
  (setq password-cache-expiry 60)  ; default is 16
  ;; string used for end of line in rsh connections
  ;; (setq tramp-rsh-end-of-line  ; `\n' by default
  ;;       (cond (running-ms-windows
  ;;              "\n")
  ;;             (t
  ;;              "\r")))
)

