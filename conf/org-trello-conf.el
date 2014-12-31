(require 'org-trello)
; (custom-set-variables '(org-trello-files '((concat org-directory "/trello/work.org")
;                                            (concat org-directory "/trello/personal.org")
;                                            (concat org-directory "/trello/blog.org")
;                                            (concat org-directory "/trello/test.org")
;                                            (concat org-directory "/trello/fabric.org"))))
; (custom-set-variables '(org-trello-files '("~/org/trello/test.org" "~/org/trello/blog.org")))
; (custom-set-variables '(org-trello-files (file-expand-wildcards "~/org/trello/*.org")))

(setq org-trello-files (file-expand-wildcards (concat org-directory "/trello/*.org")))
; (custom-set-variables '(*ORGTRELLO/MODE-PREFIX-KEYBINDING* "C-c o"))

(provide 'org-trello-conf)