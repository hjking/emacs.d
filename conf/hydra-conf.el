;; Time-stamp: <2016-01-20 19:38:12 hjking>

;; Hydra
;; https://github.com/abo-abo/hydra

(use-package hydra
  :load-path (lambda () (concat my-site-lisp-dir "hydra/"))
  :config
  (progn
    ;; (setq hydra-lv nil)
    (set-face-attribute 'hydra-face-red      nil :foreground "Red"        :bold t)
    (set-face-attribute 'hydra-face-blue     nil :foreground "RoyalBlue3" :bold t)
    (set-face-attribute 'hydra-face-amaranth nil :foreground "#e52b50"    :bold t)
    (set-face-attribute 'hydra-face-pink     nil :foreground "HotPink1"   :bold t)
    (set-face-attribute 'hydra-face-teal     nil :foreground "#367588"    :bold t)
    (hydra-add-font-lock)))

(provide 'hydra-conf)

;; |----------+-----------+------------------+----------------+-------------|
;; | Body     | Head      | Allows execution | Quits hydra    | Quits hydra |
;; | Color    | Inherited | of NON-HEADs     | after NON-HEAD | after HEAD  |
;; |          | Color     |                  | execution      | execution   |
;; |----------+-----------+------------------+----------------+-------------|
;; | amaranth | red       | No               | No             | No          |
;; | pink     | red       | Yes              | No             | No          |
;; | red      | red       | Yes              | Yes            | No          |
;; | teal     | blue      | No               | No             | Yes         |
;; | blue     | blue      | Yes              | Yes            | Yes         |
;; |----------+-----------+------------------+----------------+-------------|