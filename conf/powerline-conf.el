
(require 'powerline)

;; (setq powerline-arrow-shape 'arrow)   ;; the default
;; (setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
;; (setq powerline-arrow-shape 'arrow14) ;; best for small fonts')')')

(powerline-default-theme)

(defun my-powerline-center-theme ()
  "My powerline center theme."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'my-powerline-active 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face1 'l)
                                     (powerline-buffer-size face1 'l)
                                     (powerline-buffer-id face1 'l)
                                     (powerline-vc face1)
                                     ))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (powerline-raw "%4l" face2 'r)
                                     (powerline-raw ":" face2)
                                     (powerline-raw "%3c" face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw " " face1 'r)
                                     (powerline-raw "%6p" face1 'r)
                                     
                                     ))
                          (center (list (powerline-raw " " face1)
                                        (funcall separator-left face1 face2)
                                        (when (boundp 'erc-modified-channels-object)
                                          (powerline-raw erc-modified-channels-object face2 'l))
                                        (powerline-major-mode face2 'l)
                                        (powerline-process face2)
                                        (powerline-raw " :" face2)
                                        (powerline-minor-modes face2 'l)
                                        (powerline-raw " " face2)
                                        )))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))