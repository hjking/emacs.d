;; -*- lexical-binding: t -*-
;;
;; Filename: doc-view-conf.el
;; Description: Setting for doc-view.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:51:28
;;

(use-package doc-view
    :defer t
    :init
    (progn
      ;; DPI resolution used to render the documents
      ;; `doc-view-enlarge' (`+') and `doc-view-shrink' (`-') work fine to zoom
      ;; in or out
      (setq doc-view-resolution 96)

      ;; DPI your screen supports
      (setq doc-view-display-size 96)

      (setq doc-view-continuous t)

      ;; You can open the *text* of the current doc in a new buffer, by pressing
      ;; `C-c C-t' in doc-view-mode

      (setq doc-view-conversion-refresh-interval 3))
    )

;; Another option, without `doc-view', is `! pdtotext ? - RET'