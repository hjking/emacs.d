(use-package cool-moves
  :config
  (general-define-key
   :keymaps 'override
   "<C-down>" 'cool-moves/paragraph-forward
   "<C-up>" 'cool-moves/paragraph-backward
   "C-S-j" 'cool-moves/line-forward
   "C-S-k" 'cool-moves/line-backward
   "C-M-n" 'cool-moves/word-forward
   "C-M-p" 'cool-moves/word-backwards))

(defhydra hydra-text-motions (:color amaranth :hint nil :foreign-keys nil)
  "
    ^
  ^Motions^
  -------------------------
  _l_: line ↓      _w_: word →
  _L_: line ↑      _W_: word ←
  _p_: par  ↓      _c_: char →
  _P_: par  ↑      _C_: char ←
  _s_: sentence →  _x_: sexp →
  _S_: sentence ←  _X_: sexp ←

    "

  ("<escape>" nil)
  ("u" nil)

  ("l" cool-moves/line-forward)
  ("L" cool-moves/line-backward)

  ("p" cool-moves/paragraph-forward)
  ("P" cool-moves/paragraph-backward)

  ("w" cool-moves/word-forward)
  ("W" cool-moves/word-backwards)

  ("c" cool-moves/character-forward)
  ("C" cool-moves/character-backward)

  ("s" cool-moves/sentence-forward)
  ("S" cool-moves/sentence-backward)

  ("x" cool-moves/sexp-forward)
  ("X" cool-moves/sexp-backward))