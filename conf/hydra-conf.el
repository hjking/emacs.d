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
    (hydra-add-font-lock)

    (defhydra hydra-projectile-other-window (:color teal)
      "projectile-other-window"
      ("f"  projectile-find-file-other-window        "file")
      ("g"  projectile-find-file-dwim-other-window   "file dwim")
      ("d"  projectile-find-dir-other-window         "dir")
      ("b"  projectile-switch-to-buffer-other-window "buffer")
      ("q"  nil                                      "cancel" :color blue))

    (defhydra hydra-projectile (:color blue
                                       :hint nil)
      "
         PROJECTILE: %(projectile-project-root)
         Find File            Search/Tags          Buffers                Cache
    ------------------------------------------------------------------------------------------
    _s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
     _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
     _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
      _r_: recent file                                               ^^^^_z_: cache current
      _d_: dir
    "
      ("a"   projectile-ag)
      ("b"   projectile-switch-to-buffer)
      ("c"   projectile-invalidate-cache)
      ("d"   projectile-find-dir)
      ("s-f" projectile-find-file)
      ("ff"  projectile-find-file-dwim)
      ("fd"  projectile-find-file-in-directory)
      ("g"   ggtags-update-tags)
      ("s-g" ggtags-update-tags)
      ("i"   projectile-ibuffer)
      ("K"   projectile-kill-buffers)
      ("s-k" projectile-kill-buffers)
      ("m"   projectile-multi-occur)
      ("o"   projectile-multi-occur)
      ("s-p" projectile-switch-project "switch project")
      ("p"   projectile-switch-project)
      ("s"   projectile-switch-project)
      ("r"   projectile-recentf)
      ("x"   projectile-remove-known-project)
      ("X"   projectile-cleanup-known-projects)
      ("z"   projectile-cache-current-file)
      ("`"   hydra-projectile-other-window/body "other window")
      ("q"   nil "cancel" :color blue))

    (defhydra hydra-projectile2 (:color blue :hint nil :idle 0.4)
            "
                                                                                                                              ╭────────────┐                   ╭────────┐
       Files             Search          Buffer             Do                Other Window      Run             Cache         │ Projectile │                   │ Fixmee │
     ╭────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┴────────────╯  ╭────────────────┴────────╯
       [_f_] file (Helm)   [_a_] ag          [_b_] switch (Helm)  [_g_] magit         [_F_] file          [_U_] test        [_kc_] clear         [_x_] TODO & FIXME
       [_l_] file dwim     [_A_] grep        [_v_] show all       [_P_] commander     [_L_] dwim          [_m_] compile     [_kk_] add current   [_X_] toggle
       [_r_] recent file   [_s_] occur       [_V_] ibuffer        [_i_] info          [_D_] dir           [_c_] shell       [_ks_] cleanup
       [_d_] dir           [_S_] replace     [_K_] kill all        ^ ^                [_O_] other         [_C_] command     [_kd_] remove
        ^ ^                 ^ ^               ^ ^                  ^ ^                [_B_] buffer
       [_p_] Switch Project
      --------------------------------------------------------------------------------
            "
            ("<tab>" hydra-master/body "back")
            ("<ESC>" nil "quit")
            ("a"   helm-projectile-ag)
            ("A"   projectile-grep)
            ("B"   projectile-switch-to-buffer)
            ("b"   helm-projectile-switch-to-buffer)
            ("c"   projectile-run-async-shell-command-in-root)
            ("C"   projectile-run-command-in-root)
            ("d"   projectile-find-dir)
            ("D"   projectile-find-dir-other-window)
            ("f"   helm-projectile-find-file)
            ("F"   projectile-find-file)
            ("g"   projectile-vc)
            ("h"   projectile-dired)
            ("i"   projectile-project-info)
            ("kc"  projectile-invalidate-cache)
            ("kd"  projectile-remove-known-project)
            ("kk"  projectile-cache-current-file)
            ("p"   helm-projectile-switch-project)
            ("P"   projectile-switch-project)
            ("K"   projectile-kill-buffers)
            ("ks"  projectile-cleanup-known-projects)
            ("l"   projectile-find-file-dwim)
            ("L"   projectile-find-file-dwim-other-window)
            ("m"   projectile-compile-project)
            ("o"   projectile-find-other-file)
            ("O"   projectile-find-other-file-other-window)
            ("r"   helm-projectile-recentf)
            ("R"   projectile-recentf)
            ("s"   projectile-multi-occur)
            ("S"   projectile-replace)
            ("t"   projectile-find-tag)
            ("T"   projectile-regenerate-tags)
            ("u"   projectile-find-test-file)
            ("U"   projectile-test-project)
            ("v"   projectile-display-buffer)
            ("V"   projectile-ibuffer)
            ("X"   fixmee-mode)
            ("x"   fixmee-view-listing))

    (defhydra hydra-window (:color pink :hint nil)
      "
        Move Windows         Buffers               ^Move Frames               ^^Resize Frames
        -------------------------------------------------------------------------------------
                             _b_uffer Ido            _<up>_    Move up           _C-<up>_    Shrink V.
              ↑              _f_ind file Ido         _<down>_  Move down         _C-<down>_  Grow V.
            ←   →            _r_ecent Ido            _<left>_  Move left         _C-<left>_  Shrink H.
              ↓              _B_uffer Helm           _<right>_ Move right        _C-<right>_ Grow H.
                             _F_ind file Helm        _o_       Rotate            _=_         Balance
                             _R_recent Helm
                             _c_ider REPL
      ^_0_^ Close   _1_ Keep   _2_ Split V   _3_ Split H   _n_ Other Window   _q_ quit
    "
      ("0" delete-window)
      ("1" delete-other-windows)
      ("2" split-window-below)
      ("3" split-window-right)
      ("n" other-window true)
      ("c" cider-popup :exit true)
      ("C-S-<up>" shink-more)
      ("C-<up>" shrink-window)
      ("C-<down>" enlarge-window)
      ("C-<left>" shrink-window-horizontally)
      ("C-<right>" enlarge-window-horizontally)
      ("<up>" windmove-up)
      ("<down>" windmove-down)
      ("<left>" windmove-left)
      ("<right>" windmove-right)
      ("C-n" windmove-down)
      ("C-p" windmove-up)
      ("C-f" windmove-right)
      ("C-b" windmove-left)
      ("=" balance-windows)
      ("o" rotate-windows)
      ("r" helm-projectile-recentf)
      ("R" ido-recentf-open)
      ("b" helm-projectile-switch-to-buffer)
      ("B" ido-switch-buffer)
      ("f" helm-projectile-find-file)
      ("F" ido-find-file)
      ("q" nil :exit truex))

    )
  )

(provide 'hydra-conf)

;; https://github.com/abo-abo/hydra/wiki/Hydra-Colors#colorful-hydras
;; https://github.com/abo-abo/hydra/wiki/internals#exit
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | Body     | Non-color                   | Head      | Executing             | After     |
;; | Color    | Alternative                 | Inherited | NON-HEADS             | executing |
;; |          |                             | Color     |                       | HEADS     |
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | red      | :foreign-keys nil (default) | red       | Allow and Quit        |           |
;; |          | :exit nil (default)         |           |                       | Continue  |
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | blue     | :foreign-keys nil (default) | blue      | Allow and Quit        |           |
;; |          | :exit t                     |           |                       | Quit      |
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | amaranth | :foreign-keys warn          | red       | Disallow and Continue |           |
;; |          | :exit nil (default)         |           |                       | Continue  |
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | teal     | :foreign-keys warn          | blue      | Disallow and Continue |           |
;; |          | :exit t                     |           |                       | Quit      |
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | pink     | :foreign-keys run           | red       | Allow and Continue    |           |
;; |          | :exit nil (default)         |           |                       | Continue  |
;; |----------+-----------------------------+-----------+-----------------------+-----------|