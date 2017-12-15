;; Time-stamp: <2016-01-20 19:38:12 hjking>

;; Hydra
;; https://github.com/abo-abo/hydra

(use-package hydra
  :config
  (progn
    ;; (setq hydra-lv nil)
    ; (set-face-attribute 'hydra-face-red      nil :foreground "Red"        :bold t)
    ; (set-face-attribute 'hydra-face-blue     nil :foreground "RoyalBlue3" :bold t)
    ; (set-face-attribute 'hydra-face-amaranth nil :foreground "#e52b50"    :bold t)
    ; (set-face-attribute 'hydra-face-pink     nil :foreground "HotPink1"   :bold t)
    ; (set-face-attribute 'hydra-face-teal     nil :foreground "#367588"    :bold t)
    ;; Use a lighter red for my dark backgrounds
    (set-face-foreground 'hydra-face-red "#FF3232")
    ; (hydra-add-font-lock)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; projectile
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-projectile-other-window (:color teal)
      "projectile-other-window"
      ("f"  projectile-find-file-other-window        "file")
      ("g"  projectile-find-file-dwim-other-window   "file dwim")
      ("d"  projectile-find-dir-other-window         "dir")
      ("b"  projectile-switch-to-buffer-other-window "buffer")
      ("q"  nil                                      "cancel" :color blue))

    (defhydra hjking-hydra-projectile (:color blue :hint nil)
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
      ("s-p" projectile-switch-project)
      ("p"   projectile-switch-project)
      ("s"   projectile-switch-project)
      ("r"   projectile-recentf)
      ("x"   projectile-remove-known-project)
      ("X"   projectile-cleanup-known-projects)
      ("z"   projectile-cache-current-file)
      ("`"   hydra-projectile-other-window/body "other window" :color pink)
      ("q"   nil "cancel" :color blue))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; projectile
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-project (:color blue :hint nil :idle 0.4)
      "
                                                                        ╭────────────┐
        Files             Search          Buffer             Do          │ Projectile │
      ╭─────────────────────────────────────────────────────────────────┴────────────╯
        [_f_] file          [_a_] ag          [_b_] switch         [_g_] magit
        [_l_] file dwim     [_A_] grep        [_v_] show all       [_p_] commander
        [_r_] recent file   [_s_] occur       [_V_] ibuffer        [_i_] info
        [_d_] dir           [_S_] replace     [_K_] kill all
        [_o_] other         [_t_] find tag
        [_u_] test file     [_T_] make tags
        [_h_] root
                                                                              ╭────────┐
        Other Window      Run             Cache                 Do             │ Fixmee │
      ╭──────────────────────────────────────────────────╯ ╭────────────────┴────────╯
        [_F_] file          [_U_] test        [_kc_] clear         [_x_] TODO & FIXME
        [_L_] dwim          [_m_] compile     [_kk_] add current   [_X_] toggle
        [_D_] dir           [_c_] shell       [_ks_] cleanup
        [_O_] other         [_C_] command     [_kd_] remove
        [_B_] buffer
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

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; window
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-window (:color pink :hint nil)
      "
        Move Windows         Buffers               ^Move Frames               ^^Resize Frames
        -------------------------------------------------------------------------------------
                             _b_uffer Ido            _<up>_    Move up           _C-<up>_    Shrink V.
              ↑              _f_ind file Ido         _<down>_  Move down         _C-<down>_  Grow V.
            ←   →            _r_ecent Ido            _<left>_  Move left         _C-<left>_  Shrink H.
              ↓              _B_uffer Helm           _<right>_ Move right        _C-<right>_ Grow H.
                             _F_ind file Helm        _o_       Rotate            _=_         Balance

        ^_x_^ Close   _1_ Keep   _2_ Split V   _3_ Split H   _n_ Other Window  _z_ undo _Z_ redo _q_/_SPC_ quit
      "
      ("x"          delete-window)
      ("0"          delete-window)
      ("1"          delete-other-windows)
      ("2"          split-window-below)
      ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
      ("3"          split-window-right)
      ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
      ("n"          other-window true)
      ("c"          cider-popup :exit t)
      ("C-S-<up>"   shink-more)

      ("C-<down>"   enlarge-window)
      ("C-<right>"  enlarge-window-horizontally)
      ("C-<up>"     shrink-window)
      ("C-<left>"   shrink-window-horizontally)
      ("+"          enlarge-window)
      (">"          enlarge-window-horizontally)
      ("-"          shrink-window)
      ("<"          shrink-window-horizontally)
      ("_"          shrink-window-if-larger-than-buffer)
      ("="          balance-windows)
      ("o"          rotate-windows)

      ("<up>"       windmove-up)
      ("<down>"     windmove-down)
      ("<left>"     windmove-left)
      ("<right>"    windmove-right)
      ("C-p"        windmove-up)
      ("C-n"        windmove-down)
      ("C-b"        windmove-left)
      ("C-f"        windmove-right)

      ("z"          winner-undo)
      ("Z"          winner-redo)
      ("r"          helm-projectile-recentf)
      ("R"          ido-recentf-open)
      ("b"          helm-projectile-switch-to-buffer)
      ("B"          ido-switch-buffer)
      ("f"          helm-projectile-find-file)
      ("F"          ido-find-file)
      ("SPC"        nil)
      ("q"          nil   :exit t))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; tabbar
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-tabbar ()
      "
              switch tabbars
      "
      ("<right>" tabbar-forward "next")
      ("<left>" tabbar-backward "previous"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; zoom font
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-zoom ()
      "
              zoom
      "
      ("+" text-scale-increase     "zoom in")
      ("-" text-scale-decrease     "zoom out")
      ("=" (text-scale-adjust 0)   "="))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; buffer
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-buffer-move ()
      "
              buffer-move
      "
      ("<left>"   buf-move-left     "←")
      ("<up>"     buf-move-up       "↑")
      ("<right>"  buf-move-right    "→")
      ("<down>"   buf-move-down     "↓"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; multiple-sursor
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-multiple-cursors (:hint nil)
      "
              multiple-cursors

           ^Prev^         ^Next^        ^Other^
      ----------------------------------------------
      [_p_]   prev    [_n_]   Next    [_l_] Edit lines
      [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
      [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
      ^ ^             ^ ^             [_q_] Quit
      "
      ("a"    mc/mark-all-like-this             :exit t)
      ("n"    mc/mark-next-like-this            )
      ("N"    mc/skip-to-next-like-this)
      ("M-n"  mc/unmark-next-like-this)
      ("p"    mc/mark-previous-like-this        )
      ("P"    mc/skip-to-previous-like-this)
      ("M-p"  mc/unmark-previous-like-this)
      ("i"    mc/interactive-insert-numbers     )
      ("I"    mc/insert-numbers                 )
      ("j"    mc/cycle-forward                  )
      ("k"    mc/cycle-backward                 )
      ("l"    mc/edit-lines                     :exit t)
      ; ("u" mc/unmark-next-like-this       "ummark next")
      ; ("U" mc/unmark-previous-like-this   "unmark previous")
      ; ("s" mc/skip-to-next-like-this      "skip next")
      ; ("S" mc/skip-to-previous-like-this  "skip previous")
      ("r"    mc/mark-all-in-region-regexp      :exit t)
      ("'"    mc-hide-unmatched-lines-mode      )
      ("q"    nil))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; avy
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-avy (:color blue :hint nil)
      "
             Jump to
        ^char^                     ^word^              ^line^
        ^^^^^^^^---------------------------------------------------------
        _a_: char timer           _w_: word-0       _k_: line above
        _b_: char-2               _x_: word-1       _l_: line
        _c_: char                 _y_: subword-0    _m_: line below
        _d_: char inline          _z_: subword-1
        _e_: char or subword
        -----------------------------------------------------------------
      "
      ;; Char
      ("e" avy-goto-word-or-subword-1)
      ("c" avy-goto-char)
      ("a" avy-goto-char-timer)
      ("b" avy-goto-char-2)
      ("d" avy-goto-char-in-line)
      ;; Line
      ("l" avy-goto-line)
      ("k" avy-goto-line-above)
      ("m" avy-goto-line-below)
      ;; Word
      ("w" avy-goto-word-0)
      ("x" avy-goto-word-1)
      ("y" avy-goto-subword-1)
      ("z" avy-goto-subword-0))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; rectangle
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-rectangle (:pre (rectangle-mark-mode 1) :color pink :hint nil)
      "
        ^_k_^     _d_: delete  _s_: string   _x_: clear    _e_: exchange
      _h_   _l_   _p_: paste   _r_: replace  _I_: insert
        ^_j_^     _y_: copy    _o_: open     _R_: reset
      ^^^^        _D_: kill    _n_: number   _q_: quit
      "
      ("h" backward-char nil)
      ("l" forward-char nil)
      ("k" previous-line nil)
      ("j" next-line nil)

      ("d" delete-rectangle nil)
      ("p" yank-rectangle)
      ("y" copy-rectangle-as-kill)
      ("D" kill-rectangle nil)

      ("s" string-rectangle)
      ("r" replace-rectangle)
      ("o" open-rectangle nil)
      ("n" rectangle-number-lines)
      ("e" exchange-point-and-mark nil)

      ("x" clear-rectangle nil)
      ("I" string-insert-rectangle)
      ("R" (if (region-active-p)
               (deactivate-mark)
             (rectangle-mark-mode 1)) nil)
      ("q" keyboard-quit :color blue))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; toggle minor mode
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-toggle-mode (:color pink :hint nil)
      "
      _a_ abbrev-mode:        %`abbrev-mode
      _b_ menu-bar:           %`menu-bar-mode
      _c_ column-number-mode: %`column-number-mode
      _d_ debug-on-error:     %`debug-on-error
      _f_ auto-fill-mode:     %`auto-fill-function
      _s_ hs-minor-mode:      %`hs-minor-mode
      _r_ read-only-mode:     %`buffer-read-only
      _t_ truncate-lines:     %`truncate-lines
      _w_ wttrin:             %`weather
      _p_ list-packages:      %`paradox
      _y_ yas-global-mode:    %`yasnippet

      "
      ("a" abbrev-mode nil)
      ("b" menu-bar-mode nil)
      ("c" column-number-mode nil)
      ("d" toggle-debug-on-error nil)
      ("f" auto-fill-mode nil)
      ; ("h" highlight-indentation-mode nil)
      ; ("j" highlight-changes-mode nil)
      ("p" list-packages)
      ("t" toggle-truncate-lines nil)
      ("s" hs-minor-mode)
      ("r" read-only-mode)
      ("w" wttrin)
      ; ("w" whitespace-mode nil)
      ("y" yas-global-mode)
      ("q" nil "quit"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; buffer menu
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-buffer-menu (:color pink :hint nil)
      "
        ^Mark^             ^Unmark^           ^Actions^          ^Search
        ^^^^^^^^-----------------------------------------------------------------                        (__)
        _m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch                         (oo)
        _s_: save          _U_: unmark up     _b_: bury          _I_: isearch                      /------\\/
        _d_: delete        ^ ^                _g_: refresh       _O_: multi-occur                 / |    ||
        _D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only^^    *  /\\---/\\
        _~_: modified      ^ ^                ^ ^                ^^                                 ~~   ~~
      "
      ("m" Buffer-menu-mark)
      ("u" Buffer-menu-unmark)
      ("U" Buffer-menu-backup-unmark)
      ("d" Buffer-menu-delete)
      ("D" Buffer-menu-delete-backwards)
      ("s" Buffer-menu-save)
      ("~" Buffer-menu-not-modified)
      ("x" Buffer-menu-execute)
      ("b" Buffer-menu-bury)
      ("g" revert-buffer)
      ("T" Buffer-menu-toggle-files-only)
      ("O" Buffer-menu-multi-occur :color blue)
      ("I" Buffer-menu-isearch-buffers :color blue)
      ("R" Buffer-menu-isearch-buffers-regexp :color blue)
      ("c" nil "cancel")
      ("v" Buffer-menu-select :color blue)
      ("o" Buffer-menu-other-window  :color blue)
      ("q" quit-window "quit" :color blue))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; nivagate
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-navigate (:color pink :hint nil)
      "
          Navigate
      ^^^^^^^^-----------------------------------------------------------------------------------------------------------
      Char:        _f_: forward-char        _b_: backward-char
      Word:        _w_: forward-word        _W_: backward-word       _o_: subword-right         _O_: subword-left
      Sentence:    _s_: forward sentence    _S_: backward sentence
      Line:        _n_: next-line           _p_: previous-line       _,_: beginning-of-line     _._: end-of-line
      Paragraph:   _a_: forward paragraph   _A_: backward paragraph
      Page:        _e_: forward page        _E_: backward page       _<up>_: scroll-up          _<down>_: scroll-down
      Buffer:      _<left>_: previous       _<right>_: next          _<_: beginning of buffer   _>_: end of buffer
      sexp:        _[_: backward-sexp       _]_: forward-sexp
      Last Change: _?_: goto-last-change    _/_: goto-last-change-reverse
      "
      ;; char
      ("f" forward-char)
      ("b" backward-char)
      ;; word
      ("w" forward-word)
      ("W" backward-word)
      ("o" subword-right)
      ("O" subword-left)
      ;; line
      ("n" next-line)
      ("p" previous-line)
      ("," beginning-of-line)
      ("." end-of-line)
      ;; sentence
      ("s" forward-sentence)
      ("S" backward-sentence)
      ;; paragraph
      ("a" forward-paragraph)
      ("A" backward-paragraph)
      ;; page
      ("e" forward-page)
      ("E" backward-page)
      ("<up>" scroll-up)
      ("<down>" scroll-down)
      ;; buffer
      ("<right>" next-buffer)
      ("<left>" previous-buffer)
      ("<" beginning-of-buffer)
      (">" end-of-buffer)
      ;; sexp
      ("[" backward-sexp)
      ("]" forward-sexp)
      ;; goto last change
      ("?" goto-last-change)
      ("/" goto-last-change-reverse)

      ("i" hjking-hydra-window/body  "Window"  :color blue)
      ("g" hjking-hydra-avy/body     "Goto"    :color blue)
      ("d" hjking-hydra-delete/body  "Delete"  :color blue)
      ("q" nil                       "Quit"    :color blue)
      ; ("e" move-end-of-line)
      ; ("v" scroll-up-command)
      ; ("V" scroll-down-command)
      ("l" recenter-top-bottom))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; delete
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-delete (:hint nil)
      "
          Delete
        _f_/_C-d_: delete-char       _w_: kill-word            _l_: kill-line
        _b_: backward-delete-char    _W_: backward-kill-word   _L_: kill-line backward
        _s_: kill-sentence           _p_: kill-paragraph       _x_: kill-sexp
      "
      ("f" delete-char)
      ("C-d" delete-char)
      ("d" delete-char)
      ("b" backward-delete-char)
      ("w" kill-word)
      ("W" backward-kill-word)
      ("l" kill-line)
      ("L" (progn  (set-mark (point)) (beginning-of-line) (kill-region (point) (mark))))
      ("p" kill-paragraph)
      ("s" kill-sentence)
      ("x" kill-sexp)
      ("n" hjking-hydra-navigate/body   "navigate"  :color blue)
      ("a" hjking-hydra-avy/body        "Goto"      :color blue)
      ("i" hjking-hydra-window/body     "Window"    :color blue)
      ("q" nil "Quit" :color blue))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; markdown mode
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-markdown-mode (:hint nil)
      "
      Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code
      Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4
      Lists             C-c C-x    _m_: insert item
      Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down
      Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference
      "
      ("s" markdown-insert-bold)
      ("e" markdown-insert-italic)
      ("b" markdown-insert-blockquote :color blue)
      ("p" markdown-insert-pre :color blue)
      ("c" markdown-insert-code)

      ("h" markdown-insert-header-dwim)
      ("1" markdown-insert-header-atx-1)
      ("2" markdown-insert-header-atx-2)
      ("3" markdown-insert-header-atx-3)
      ("4" markdown-insert-header-atx-4)

      ("m" markdown-insert-list-item)

      ("l" markdown-promote)
      ("r" markdown-demote)
      ("d" markdown-move-down)
      ("u" markdown-move-up)

      ("L" markdown-insert-link                :color blue)
      ("U" markdown-insert-uri                 :color blue)
      ("F" markdown-insert-footnote            :color blue)
      ("W" markdown-insert-wiki-link           :color blue)
      ("R" markdown-insert-reference-link-dwim :color blue))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; ivy
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-ivy (:hint nil)
      "
       Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
      ----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
       _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
       ^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
       _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
      "
      ;; arrows
      ("j" ivy-next-line)
      ("k" ivy-previous-line)
      ("l" ivy-alt-done)
      ("h" ivy-backward-delete-char)
      ("g" ivy-beginning-of-buffer)
      ("G" ivy-end-of-buffer)
      ("d" ivy-scroll-up-command)
      ("u" ivy-scroll-down-command)
      ("e" ivy-scroll-down-command)
      ;; actions
      ("q" keyboard-escape-quit :exit t)
      ("C-g" keyboard-escape-quit :exit t)
      ("<escape>" keyboard-escape-quit :exit t)
      ("C-o" nil)
      ("i" nil)
      ("TAB" ivy-alt-done :exit nil)
      ("C-j" ivy-alt-done :exit nil)
      ;; ("d" ivy-done :exit t)
      ("RET" ivy-done :exit t)
      ("C-m" ivy-done :exit t)
      ("f" ivy-call)
      ("c" ivy-toggle-calling)
      ("m" ivy-toggle-fuzzy)
      (">" ivy-minibuffer-grow)
      ("<" ivy-minibuffer-shrink)
      ("w" ivy-prev-action)
      ("s" ivy-next-action)
      ("a" ivy-read-action)
      ("t" (setq truncate-lines (not truncate-lines)))
      ("C" ivy-toggle-case-fold)
      ("o" ivy-occur :exit t))
    (with-eval-after-load 'ivy
      (define-key ivy-minibuffer-map "\C-o" 'hjking-hydra-ivy/body))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; yasnippet
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-yasnippet (:hint nil)
      "
                    ^YASnippets^
      --------------------------------------------
        Modes:    Load/Visit:    Actions:

       _g_lobal    _d_irectory    _i_nsert
       _m_inor     _f_ile         _t_ryout
       _e_xtra     _l_ist         _n_ew
                   _a_ll
      "
      ("d" yas-load-directory)
      ("e" yas-activate-extra-mode)
      ("i" yas-insert-snippet)
      ("f" yas-visit-snippet-file)
      ("n" yas-new-snippet)
      ("t" yas-tryout-snippet)
      ("l" yas-describe-tables)
      ("g" yas/global-mode)
      ("m" yas/minor-mode)
      ("a" yas-reload-all))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; fold
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-fold (:pre (hs-minor-mode 1))
      "fold"
      ("t" fold-dwim-toggle "toggle")
      ("h" fold-dwim-hide-all "hide-all")
      ("s" fold-dwim-show-all "show-all")
      ("q" nil "quit"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; ibuffer
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hydra-ibuffer-main (:hint nil)
      "
       ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
      -^----------^-+-^----^--------+-^-------^--------+-^----^-------
        _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
       _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
        _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
      -^----------^-+-^----^--------+-^-------^--------+-^----^-------
      "
      ("j" ibuffer-forward-line)
      ("RET" ibuffer-visit-buffer :color blue)
      ("k" ibuffer-backward-line)

      ("m" ibuffer-mark-forward)
      ("u" ibuffer-unmark-forward)
      ("*" hydra-ibuffer-mark/body :color blue)

      ("D" ibuffer-do-delete)
      ("S" ibuffer-do-save)
      ("a" hydra-ibuffer-action/body :color blue)

      ("g" ibuffer-update)
      ("s" hydra-ibuffer-sort/body :color blue)
      ("/" hydra-ibuffer-filter/body :color blue)

      ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
      ("q" nil "quit ibuffer" :color blue)
      ("." nil "toggle hydra" :color blue))

    (defhydra hydra-ibuffer-mark (:color teal :columns 4
                                  :after-exit (hydra-ibuffer-main/body))
      "Mark"
      ("*" ibuffer-unmark-all "unmark all")
      ("M" ibuffer-mark-by-mode "mode")
      ("m" ibuffer-mark-modified-buffers "modified")
      ("u" ibuffer-mark-unsaved-buffers "unsaved")
      ("s" ibuffer-mark-special-buffers "special")
      ("r" ibuffer-mark-read-only-buffers "read-only")
      ("/" ibuffer-mark-dired-buffers "dired")
      ("e" ibuffer-mark-dissociated-buffers "dissociated")
      ("h" ibuffer-mark-help-buffers "help")
      ("z" ibuffer-mark-compressed-file-buffers "compressed")
      ("b" hydra-ibuffer-main/body "back" :color blue))

    (defhydra hydra-ibuffer-action (:color teal :columns 4
                                    :after-exit
                                    (if (eq major-mode 'ibuffer-mode)
                                        (hydra-ibuffer-main/body)))
      "Action"
      ("A" ibuffer-do-view "view")
      ("E" ibuffer-do-eval "eval")
      ("F" ibuffer-do-shell-command-file "shell-command-file")
      ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
      ("H" ibuffer-do-view-other-frame "view-other-frame")
      ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
      ("M" ibuffer-do-toggle-modified "toggle-modified")
      ("O" ibuffer-do-occur "occur")
      ("P" ibuffer-do-print "print")
      ("Q" ibuffer-do-query-replace "query-replace")
      ("R" ibuffer-do-rename-uniquely "rename-uniquely")
      ("T" ibuffer-do-toggle-read-only "toggle-read-only")
      ("U" ibuffer-do-replace-regexp "replace-regexp")
      ("V" ibuffer-do-revert "revert")
      ("W" ibuffer-do-view-and-eval "view-and-eval")
      ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
      ("b" hydra-ibuffer-main/body "back" :color blue))

    (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
      "Sort"
      ("i" ibuffer-invert-sorting "invert")
      ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
      ("v" ibuffer-do-sort-by-recency "recently used")
      ("s" ibuffer-do-sort-by-size "size")
      ("f" ibuffer-do-sort-by-filename/process "filename")
      ("m" ibuffer-do-sort-by-major-mode "mode")
      ("b" hydra-ibuffer-main/body "back" :color blue))

    (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
      "Filter"
      ("m" ibuffer-filter-by-used-mode "mode")
      ("M" ibuffer-filter-by-derived-mode "derived mode")
      ("n" ibuffer-filter-by-name "name")
      ("c" ibuffer-filter-by-content "content")
      ("e" ibuffer-filter-by-predicate "predicate")
      ("f" ibuffer-filter-by-filename "filename")
      (">" ibuffer-filter-by-size-gt "size")
      ("<" ibuffer-filter-by-size-lt "size")
      ("/" ibuffer-filter-disable "disable")
      ("b" hydra-ibuffer-main/body "back" :color blue))

    (with-eval-after-load 'ibuffer
      (define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
      (add-hook 'ibuffer-hook #'hydra-ibuffer-main/body))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; macro
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-macro (:hint nil :pre
                                   (when defining-kbd-macro
                                       (kmacro-end-macro 1)))
      "
        ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
      ╭─────────────────────────────────────────────────────────────────────────╯
           ^_i_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
           ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
       _j_ ←   → _l_       [_o_] edit       [_a_] add       [_x_] register
           ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
           ^_k_^           [_m_] step
          ^^   ^^          [_s_] swap
      "
      ("j" kmacro-start-macro :color blue)
      ("l" kmacro-end-or-call-macro-repeat)
      ("i" kmacro-cycle-ring-previous)
      ("k" kmacro-cycle-ring-next)
      ("r" apply-macro-to-region-lines)
      ("d" kmacro-delete-ring-head)
      ("e" kmacro-end-or-call-macro-repeat)
      ("o" kmacro-edit-macro-repeat)
      ("m" kmacro-step-edit-macro)
      ("s" kmacro-swap-ring)
      ("n" kmacro-insert-counter)
      ("t" kmacro-set-counter)
      ("a" kmacro-add-counter)
      ("f" kmacro-set-format)
      ("b" kmacro-name-last-macro)
      ("K" kmacro-bind-to-key)
      ("B" insert-kbd-macro)
      ("x" kmacro-to-register)
      ("'" kmacro-edit-macro)
      ("," edit-kbd-macro)
      ("q" nil :color blue))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; info
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-info (:hint nil)
      "
              Info-mode:

        ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
        ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
        ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
        ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
        regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos

        _1_ .. _9_ Pick first .. ninth item in the node's menu.

      "
      ("]"   Info-forward-node)
      ("["   Info-backward-node)
      ("n"   Info-next)
      ("p"   Info-prev)
      ("s"   Info-search)
      ("S"   Info-search-case-sensitively)

      ("l"   Info-history-back)
      ("r"   Info-history-forward)
      ("H"   Info-history)
      ("t"   Info-top-node)
      ("<"   Info-top-node)
      (">"   Info-final-node)

      ("u"   Info-up)
      ("^"   Info-up)
      ("m"   Info-menu)
      ("g"   Info-goto-node)
      ("b"   beginning-of-buffer)
      ("e"   end-of-buffer)

      ("f"   Info-follow-reference)
      ("i"   Info-index)
      (","   Info-index-next)
      ("I"   Info-virtual-index)

      ("T"   Info-toc)
      ("d"   Info-directory)
      ("c"   Info-copy-current-node-name)
      ("C"   clone-buffer)
      ("a"   info-apropos)

      ("1"   Info-nth-menu-item)
      ("2"   Info-nth-menu-item)
      ("3"   Info-nth-menu-item)
      ("4"   Info-nth-menu-item)
      ("5"   Info-nth-menu-item)
      ("6"   Info-nth-menu-item)
      ("7"   Info-nth-menu-item)
      ("8"   Info-nth-menu-item)
      ("9"   Info-nth-menu-item)

      ("?"   Info-summary "Info summary")
      ("h"   Info-help "Info help")
      ("q"   Info-exit "Info exit")
      ("C-g" nil "cancel" :color blue))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; theme
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-themes (:hint nil :exit t)
      "
            Themes
      _c_: cyberpunk   _l_: solarized-light    _d_: solarized-dark
      _g_: gotham      _m_: monokai            _z_: zenburn
      "
      ("SPC" nil "Quit")
      ("c"
           (lambda ()
             (interactive)
             (load-theme 'cyberpunk t)
             )
           )
      ("l"
           (lambda ()
             (interactive)
             (load-theme 'solarized-light t)
             )
           )
      ("d"
           (lambda ()
             (interactive)
             (load-theme 'solarized-dark t)
             )
           )
      ("g"
           (lambda ()
             (interactive)
             (load-theme 'gotham t)
             )
           )
      ("m"
           (lambda ()
             (interactive)
             (load-theme 'monokai t)
             )
           )
      ("z"
           (lambda ()
             (interactive)
             (load-theme 'zenburn t)
             )
           )
      )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; git
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-git (:color blue :columns 4 :exit t)
      "
                  magit
      "
      ("s" magit-status "status")
      ("d" magit-diff-unstaged "diff")
      ("D" magit-diff-staged "diff cached")
      ("c" magit-commit "commit")
      ("l" magit-log "log")
      ("L" magit-log-long "log long")
      ("p" magit-push "push")
      ("u" magit-pull "pull")
      ("b" magit-branch-manager "branches")
      ("q" nil "Quit"))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; verilog
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; (defhydra hjking-hydra-verilog (:foreign-keys warn :exit t :hint nil)
    ;   "
    ;       Main menu
    ;   _r_: VHDL Compile    _c_: Compose
    ;   _p_: Port            _p_: VHDL Port
    ;   "
    ;   ("r" vhdl-compile)
    ;   ("p" hjking-hydra-verilog-port/body)
    ;   ("c" hjking-hydra-verilog-component/body)
    ;   ("q" nil "Quit")
    ;   )

    (defhydra hjking-hydra-verilog-mode-main (:foreign-keys warn :exit t :hint nil)
      "
          Verilog Mode
      _a_: auto   _h_: Help  _q_: Quit
      "
      ("a" hjking-hydra-verilog-mode-auto/body)
      ; ("d" hjking-hydra-verilog-mode-diff/body)
      ("h" verilog-faq)
      ("q" nil)
      )

    (defhydra hjking-hydra-verilog-mode-auto (:foreign-keys warn :exit t :hint nil)
      "
          Verilog Mode - auto
        _a_: auto      ^^ _g_: auto arg    ^^ _m_: comment  ^^ _u_: uncomment
        _c_: diff auto ^^ _d_: delete auto ^^  _i_: inject auto
      "
      ("a" verilog-auto)
      ("c" verilog-diff-auto)
      ("d" verilog-delete-auto)
      ("i" verilog-inject-auto)
      ("g" verilog-auto-arg)
      ("m" verilog-comment-region)
      ("u" verilog-uncomment-region)
      ("q" nil "Quit")
      ("b" hjking-hydra-verilog-mode-main/body "Back"))

    ; (defhydra hjking-hydra-verilog-mode-diff (:foreign-keys warn :exit t :hint nil))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; vhdl
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-vhdl (:foreign-keys warn :exit t :hint nil)
      "
          Main menu
      _r_: VHDL Compile    _c_: Compose
      _p_: Port            _p_: VHDL Port
      "
      ("r" vhdl-compile)
      ("p" hjking-hydra-vhdl-port/body)
      ("c" hjking-hydra-vhdl-component/body)
      ("q" nil "Quit")
      )

    (defhydra hjking-hydra-vhdl-port (:foreign-keys warn :exit t :hint nil)
      "
          Port
      _c_: VHDL Port Copy     _C_: Past As Component
                              _i_: Past As Instance
                              _p_: Past Port
                              _m_: Past PortMap
      "
      ("c" vhdl-port-copy)
      ("C" vhdl-port-paste-component)
      ("i" vhdl-port-paste-instance)
      ("p" vhdl-port-paste-port)
      ("m" vhdl-port-paste-port-map)
      ("q" nil "Quit")
      ("b" hjking-hydra-vhdl/body "Back"))

    (defhydra hjking-hydra-vhdl-component (:foreign-keys warn :exit t :hint nil)
      "
          Port
      _c_: Architecture      : Past As Compose
      _p_: Package           : Past As Instance
      _h_: Hierarchal        : Past Port
      _C_: Create File
      "
      ("c" vhdl-compose-architecture-name)
      ("p" vhdl-compose-components-package)
      ("h" vhdl-compose-configuration-hierarchical)
      ("C" vhdl-compose-create-files)
      ("q" nil "Quit")
      ("b" hjking-hydra-vhdl/body "Back"))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; org
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defhydra hjking-hydra-org (:exit t :hint nil)
      "
      ^Agend^       ^Clock^    ^Subtree^     ^Insert^               ^Capture^
      _a_: agenda   _i_: in    _r_: refile   _t_: timestamp         _z_/_w_: capture
          ^^        _o_: out   _c_: archive  _T_: active timestamp
      "
      ("a" hjking-hydra-org-agenda-view/body)
      ("z" org-capture)
      ("w" org-capture)
      ("i" org-clock-in)
      ("o" org-agenda-clock-out)
      ("t" org-time-stamp-inactive)
      ("T" org-time-stamp)
      ("r" org-refile)
      ("c" org-archive-subtree))

    (defhydra hjking-hydra-org-agenda-view (:color red
                                            :hint nil)
      "
     _d_: day        _g_: time grid    _a_: arch-trees    _L_: log closed clock
     _w_: week       _i_: inactive     _A_: arch-files    _c_: log clock check
     _t_: fortnight  _f_: follow       _r_: report        _l_: log mode toggle
     _m_: month      _e_: entry        _D_: diary         _q_: quit
     _y_: year       _!_: deadlines    _R_: reset
    "
      ("R" org-agenda-reset-view)
      ("d" org-agenda-day-view)
      ("w" org-agenda-week-view)
      ("t" org-agenda-fortnight-view)
      ("m" org-agenda-month-view)
      ("y" org-agenda-year-view)
      ("l" org-agenda-log-mode)
      ("L" (org-agenda-log-mode '(4)))
      ("c" (org-agenda-log-mode 'clockcheck))
      ("f" org-agenda-follow-mode)
      ("a" org-agenda-archives-mode)
      ("A" (org-agenda-archives-mode 'files))
      ("r" org-agenda-clockreport-mode)
      ("e" org-agenda-entry-text-mode)
      ("g" org-agenda-toggle-time-grid)
      ("D" org-agenda-toggle-diary)
      ("!" org-agenda-toggle-deadlines)
      ("i"
       (let ((org-agenda-include-inactive-timestamps t))
         (org-agenda-check-type t 'timeline 'agenda)
         (org-agenda-redo)))
      ("q" nil :color blue))

    (defhydra hjking-hydra-org-tables (:color red
                                       :hint nil)
      "
     ^Field^   ^Shift^   ^Insert^      ^Delete^         ^Field^     ^Table^      ^Formula^
    ^^^^^^^^^^^^------------------------------------------------------------------------------
     ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   _r_: row      _dr_: del row    _e_: edit   _a_: align   _+_: sum    _q_: quit
     _h_ ^+^ _l_   _H_ ^+^ _L_   _c_: column   _dc_: del col    _b_: blank  _|_: create  _=_: eval
     ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   _-_: hline                   _i_: info             _f_: edit
    "
      ("a" org-table-align)
      ("l" org-table-next-field)
      ("h" org-table-previous-field)
      ("j" org-table-end-of-field)
      ("k" org-table-beginning-of-field)
      ("r" org-table-insert-row)
      ("c" org-table-insert-column)
      ("-" org-table-insert-hline)
      ("J" org-table-move-row-down)
      ("K" org-table-move-row-up)
      ("H" org-table-move-column-left)
      ("L" org-table-move-column-right)
      ("dr" org-table-kill-row)
      ("dc" org-table-delete-column)
      ("b" org-table-blank-field)
      ("e" org-table-edit-field)
      ("i" org-table-field-info)
      ("+" org-table-sum)
      ("=" org-table-eval-formula)
      ("f" org-table-edit-formulas)
      ("|" org-table-create-or-convert-from-region)
      ("q" nil :color blue))

    (defhydra hjking-hydra-org-jump (:color pink
                                 :hint nil)
      "
     ^Outline^          ^Item^   ^Table^   ^Block^   ^Link^
     ^^^^^^^^^^^-------------------------------------------------------------------------------
     ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   ^ ^ _u_ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ _p_ ^ ^   ^ ^ _P_ ^ ^    _q_ quit
     _h_ ^+^ _l_   ^ ^ ^+^ ^ ^   ^ ^ ^+^ ^ ^   _b_ ^+^ _f_   ^ ^ ^+^ ^ ^   ^ ^ ^+^ ^ ^
     ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   ^ ^ _d_ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ _n_ ^ ^   ^ ^ _N_ ^ ^
    "
      ("j" outline-next-visible-heading)
      ("k" outline-previous-visible-heading)
      ("l" org-down-element)
      ("h" org-up-element)
      ("J" org-forward-heading-same-level)
      ("K" org-backward-heading-same-level)
      ("u" org-next-item)
      ("d" org-previous-item)
      ("f" org-table-next-field)
      ("b" org-table-previous-field)
      ("n" org-next-block)
      ("p" org-previous-block)
      ("N" org-next-link)
      ("P" org-previous-link)
      ("q" nil :color blue))

    (defhydra hjking-hydra-buffer (:color blue :columns 3)
      "
                    Buffers :
      "
      ("n" next-buffer "next" :color red)
      ("b" ivy-switch-buffer "switch")
      ("B" ibuffer "ibuffer")
      ("p" previous-buffer "prev" :color red)
      ("C-b" buffer-menu "buffer menu")
      ("N" evil-buffer-new "new")
      ("d" kill-this-buffer "delete" :color red)
      ;; don't come back to previous buffer after delete
      ("D" (progn (kill-this-buffer) (next-buffer)) "Delete" :color red)
      ("s" save-buffer "save" :color red))

    (defhydra hjking-hydra-bookmarks (:color blue :hint nil)
      "
      _s_: set  _b_: bookmark   _j_: jump   _d_: delete   _q_: quit
      "
      ("s" bookmark-set)
      ("b" bookmark-save)
      ("j" bookmark-jump)
      ("d" bookmark-delete)
      ("q" nil :color blue))

    (defhydra hjking-hydra-transpose (:color blue :hint nil)
      ("c" transpose-chars "Transpose Chars")
      ("w" transpose-words "Transpose Words")
      ("l" transpose-lines "Transpose Lines")
      ("e" transpose-sexps "Transpose Sexps")
      ("s" transpose-sentences "Transpose Sentences")
      ("p" transpose-paragraphs "Transpose Paragraphs")
      ("q" nil :color blue))

    (defhydra hjking-hydra-smartparens  (:color blue :hint nil)
      ("j" sp-down-sexp   "move down")
      ("k" sp-backward-up-sexp    "move backward up")
      ("h" sp-backward-down-sexp    "move backward down")
      ("l" sp-up-sexp   "move up")
      ("f" sp-forward-sexp    "move forward")
      ("b" sp-backward-sexp   "move backward")
      ("a" sp-beginning-of-sexp   "move beginning")
      ("e" sp-end-of-sexp   "move end")
      ("n" sp-next-sexp   "move next")
      ("p" sp-previous-sexp   "move previous")
      (">" sp-forward-barf-sexp   "expression forward barf")
      ("<" sp-backward-barf-sexp    "expression backward barf")
      (")" sp-forward-slurp-sexp    "expression forward slurp")
      ("(" sp-backward-slurp-sexp   "expression backward slurp")
      ("x" sp-transpose-sexp    "smart transpose")
      ("d" sp-kill-sexp   "smart delete")
      ("y" sp-copy-sexp   "smart copy")
      ("u" sp-unwrap-sexp   "selection unwrap")
      ("U" sp-backward-unwrap-sexp    "backward unwrap")
      ("C" sp-convolute-sexp    "convolute sexp")
      ("r" sp-raise-sexp    "raise sexp")
      ("s" sp-split-sexp    "split sexp")
      ("S" sp-splice-sexp   "splice sexp")
      ("F" sp-splice-sexp-killing-forward   "splice forward")
      ("B" sp-splice-sexp-killing-backward    "splice backward")
      ("A" sp-splice-sexp-killing-around    "splice around")
      ("q" nil :color blue))

    (defhydra hjking-hydra-align (:color blue :hint nil)
      ("SPC"     sk/align-whitespace       "align based on spaces")
      ("&"       sk/align-ampersand        "align based on &")
      (","       sk/align-comma            "align based on ,")
      ("\""      sk/align-quote-space      "align based on \"")
      ("."       sk/align-dot              "align based on .")
      ("="       sk/align-equals           "align based on =")
      (":"       sk/align-colon            "align based on :")
      ("A"       align-regexp              "align based on regex")
      ("q" nil :color blue))

    (defhydra hjking-hydra-expand (:pre (er/mark-word)
                                   :color red
                                   :hint nil)
      "
     _a_: add    _r_: reduce   _q_: quit
     "
      ("a" er/expand-region)
      ("r" er/contract-region)
      ("q" nil :color blue))

    ; (defhydra hjking-hydra-macros (:color pink
    ;                                :hint nil)
    ;   "
    ;  _m_: macro  _L_: lossage  _v_: view      _n_: forward    _D_: delete   _q_: quit
    ;  _M_: prev   _E_: edit     _r_: register  _p_: backward   _K_: key
    ;   "
    ;   ("m" kmacro-call-macro)
    ;   ("M" kmacro-call-ring-2nd)
    ;   ("L" kmacro-edit-lossage :color blue)
    ;   ("E" kmacro-edit-macro :color blue)
    ;   ("v" kmacro-view-macro :color blue)
    ;   ("r" kmacro-to-register :color blue)
    ;   ("n" kmacro-cycle-ring-next)
    ;   ("p" kmacro-cycle-ring-previous)
    ;   ("D" kmacro-delete-ring-head :color blue)
    ;   ("K" kmacro-bind-to-key :color blue)
    ;   ("q" nil :color blue))

    (defhydra hjking-hydra-registers (:color blue
                                      :hint nil)
      "
     _a_: append     _c_: copy-to    _j_: jump       _r_: rectangle-copy   _q_: quit
     _i_: insert     _n_: number-to  _f_: frameset   _w_: window-config
     _+_: increment  _p_: point-to
      "
      ("a" append-to-register)
      ("c" copy-to-register)
      ("i" insert-register)
      ("f" frameset-to-register)
      ("j" jump-to-register)
      ("n" number-to-register)
      ("r" copy-rectangle-to-register)
      ("w" window-configuration-to-register)
      ("+" increment-register)
      ("p" point-to-register)
      ("q" nil :color blue))

    (defhydra hjking-hydra-vimish-fold (:color red
                                        :hint nil)
      "
     _f_: fold  _u_: unfold  _r_: refold  _t_: toggle  _d_: delete    _n_: next      _q_: quit
              _U_: Unfold  _R_: Refold  _T_: Toggle  _D_: Delete    _p_: previous
      "
      ("f" vimish-fold)
      ("u" vimish-fold-unfold)
      ("r" vimish-fold-refold)
      ("t" vimish-fold-toggle)
      ("d" vimish-fold-delete)
      ("U" vimish-fold-unfold-all)
      ("R" vimish-fold-refold-all)
      ("T" vimish-fold-toggle-all)
      ("D" vimish-fold-delete-all)
      ("n" vimish-fold-next-fold)
      ("p" vimish-fold-previous-fold)
      ("q" nil :color blue))

    (defhydra hjking-hydra-for-elisp (:color red
                                      :hint nil)
      "
     ^Send^                         ^Navigate^                       ^Documentation^
    ^^^^^^^^^^-----------------------------------------------------------------------------------
     _r_: region    _e_: expression   _i_: library    _D_: defun         _q_: quit
     _f_: func      _l_: last sexp    _d_: defun      _n_: find dired    _V_: variable
     _b_: buffer    _s_: ielm         _v_: variable   _g_: grep          _I_: library
    "
      ("r" eval-region)
      ("f" eval-defun)
      ("b" eval-buffer)
      ("e" eval-expression :color blue)
      ("l" eval-last-sexp)
      ("s" ielm :color blue)
      ("i" find-library)
      ("d" find-function-at-point)
      ("v" find-variable)
      ("n" find-dired)
      ("g" find-grep-dired)
      ("D" describe-function)
      ("V" describe-variable)
      ("I" describe-library)
      ("q" nil :color blue))


    (defhydra hjking-hydra-highlight-symbol
     (:color red  :hint nil :post (progn (highlight-symbol-remove-all)))
     "
        highlight-symbol

  ^Highlight^          ^Next^              ^Prev
  ^^^^^^^^--------------------------------------------
  _._: highlight       _n_: next           _p_: prev
    ^ ^                _N_: next in defun  _P_: prev in defun
  "
     ("." highlight-symbol-at-point)
     ("n" highlight-symbol-next)
     ("p" highlight-symbol-prev)
     ("N" highlight-symbol-next-in-defun)
     ("P" highlight-symbol-prev-in-defun)
     ("r" highlight-symbol-query-replace  "replace" :color red)
     ("q" nil                             "cancel" :color blue))



    (defun my/hl-anything (&optional arg)
         "Wrapper function to call functions to highlight the thing at point either
     globally or locally (when called with prefix `C-u')."
         (interactive "p")
         (if (eq arg 4)
             (hl-highlight-thingatpt-local)
           (hl-highlight-thingatpt-global)))

    (defun my/unhl-anything (&optional arg)
         "Wrapper function to call functions to unhighlight all either
     globally or locally (when called with prefix `C-u')."
         (interactive "p")
         (if (eq arg 4)
             (hl-unhighlight-all-local)
           (hl-unhighlight-all-global)))

    (defhydra hjking-hydra-hl-anything (:color red :hint nil)
       "
        hl-anything
    ^Highlight^          ^UnHighlight^        ^Next/Prev^        ^Misc
    ^^^^^^^^-----------------------------------------------------------------
    _H_: global          _U_: unhl-global     _n_: next          _s_: save
    _h_: local           _h_: unhl-local      _p_: prev          _r_: restore
    "
       ("H" my/hl-anything             )
       ("h" (my/hl-anything 4)         )
       ("U" my/unhl-anything           )
       ("u" (my/unhl-anything 4)       )
       ("n" hl-find-next-thing         )
       ("p" hl-find-prev-thing         )
       ("s" hl-save-highlights         )
       ("r" hl-restore-highlights      )
       ("t" hl-global-highlight-on/off "on/off" :color blue)
       ("q" nil                        "cancel" :color blue)
       )

    ;; Hydra - Marking
    (defhydra hjking-hydra-mark (:exit t
                                :columns 3
                                :idle 1.0)
      "Mark"
      ("d" er/mark-defun "Defun / Function")
      ("f" er/mark-defun "Defun / Function")
      ("F" er/mark-clj-function-literal "Clj anonymous fn")
      ("w" er/mark-word "Word")
      ("W" er/mark-clj-word "CLJ word")
      ("u" er/mark-url "Url")
      ("e" mark-sexp "S-Expression")
      ("E" er/mark-email "Email")
      ("b" hydra-mark-buffer/body "Buffer")
      ("l" mark-line "Line")
      ("p" er/mark-text-paragraph "Paragraph")
      ("r" er/mark-clj-regexp-literal "Clj regexp")
      ("s" er/mark-symbol "Symbol")
      ("S" er/mark-symbol-with-prefix "Prefixed symbol")
      ("q" er/mark-inside-quotes "Inside quotes")
      ("Q" er/mark-outside-quotes "Outside quotes")
      ("(" er/mark-inside-pairs "Inside pairs")
      ("[" er/mark-inside-pairs "Inside pairs")
      ("{" er/mark-inside-pairs "Inside pairs")
      (")" er/mark-outside-pairs "Outside pairs")
      ("]" er/mark-outside-pairs "Outside pairs")
      ("}" er/mark-outside-pairs "Outside pairs")
      ("t" er/mark-inner-tag "Inner tag")
      ("T" er/mark-outer-tag "Outer tag")
      ("c" er/mark-comment "Comment")
      ("a" er/mark-html-attribute "HTML attribute")
      ("." er/expand-region "Expand region" :exit nil)
      ("," er/contract-region "Contract region" :exit nil)
      ("#" er/mark-clj-set-literal "Clj set")
      )

    (defhydra hjking-hydra-mark-buffer (:exit t
                                        :idle 1.0)
      "Mark buffer"
      ("w" mark-whole-buffer "Whole buffer")
      ("a" mark-buffer-after-point "Buffer after point")
      ("b" mark-buffer-before-point "Buffer before point"))

    (defhydra hydra-lisp-eval (:exit t
                               :columns 2
                               :idle 1.0)
      "Lisp eval"
      ("r" eval-region "Region")
      ("b" eval-buffer "Buffer")
      ("e" eval-expression "S-expression")
      ("l" eval-last-sexp "Last s-expression")
      ("L" eval-last-sexp-print-value "Last s-expression and print value")
      ("d" eval-defun "Defun / Function")
      ("f" eval-defun "Defun / Function"))


    (defhydra hydra-undo-tree (:color yellow
                                      :hint nil
                                      )
      "
      _p_: undo  _n_: redo _s_: save _l_: load   "
      ("p"   undo-tree-undo)
      ("n"   undo-tree-redo)
      ("s"   undo-tree-save-history)
      ("l"   undo-tree-load-history)
      ("u"   undo-tree-visualize "visualize" :color blue)
      ("q"   nil "quit" :color blue))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Key Binding
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-key hjking-mode-map "a" 'hjking-hydra-avy/body)
    (define-key hjking-mode-map "b" 'hjking-hydra-buffer-move/body)
    (define-key hjking-mode-map "c" 'hjking-hydra-multiple-cursors/body)
    (define-key hjking-mode-map "d" 'hjking-hydra-delete/body)
    (define-key hjking-mode-map "f" 'hjking-hydra-fold/body)
    (define-key hjking-mode-map "h" 'hjking-hydra-hl-anything/body)
    (define-key hjking-mode-map "l" 'hjking-hydra-highlight-symbol/body)
    (define-key hjking-mode-map "g" 'hjking-hydra-git/body)
    (define-key hjking-mode-map "m" 'hjking-hydra-markdown-mode/body)
    (define-key hjking-mode-map "n" 'hjking-hydra-navigate/body)
    (define-key hjking-mode-map "o" 'hjking-hydra-org/body)
    (define-key hjking-mode-map "p" 'hjking-hydra-projectile/body)
    (define-key hjking-mode-map "r" 'hjking-hydra-rectangle/body)
    ; (define-key hjking-mode-map "s" 'hjking-hydra-/body)
    (define-key hjking-mode-map "t" 'hjking-hydra-toggle-mode/body)
    (define-key hjking-mode-map "w" 'hjking-hydra-window/body)
    (define-key hjking-mode-map "v" 'hjking-hydra-verilog-mode-main/body)
    (define-key hjking-mode-map "y" 'hjking-hydra-yasnippet/body)
    (define-key hjking-mode-map "z" 'hjking-hydra-zoom/body)
    (define-key hjking-mode-map "." 'hjking-hydra-buffer-menu/body)
    (define-key hjking-mode-map "@" 'hjking-hydra-macro/body)
    (define-key hjking-mode-map "\"" 'hjking-hydra-registers/body)

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