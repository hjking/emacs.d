;;
;; Filename: doc-view-conf.el
;; Description: Setting for doc-view.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:51:28
;;
(message "%d: >>>>> Loading [ doc view ] Customizations ...." step_no)
(setq step_no (1+ step_no))

;; view PDF/PostScript/DVI files in Emacs
(when (require 'doc-view nil t)

  ;; `doc-view' integrates with the usual bookmark facility. So simply use
  ;; `C-x r m' (`bookmark-set') to jump back to the last page you've read
  ;; in a PDF document.


  ;; DPI resolution used to render the documents
  ;; `doc-view-enlarge' (`+') and `doc-view-shrink' (`-') work fine to zoom
  ;; in or out
  (setq doc-view-resolution 96)

  ;; DPI your screen supports
  (setq doc-view-display-size 96)

  ;; You can open the *text* of the current doc in a new buffer, by pressing
  ;; `C-c C-t' in doc-view-mode

  (setq doc-view-conversion-refresh-interval 3)
)

;; Another option, without `doc-view', is `! pdtotext ? - RET'