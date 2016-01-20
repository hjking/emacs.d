
(with-eval-after-load 'semantic
    (add-to-list 'semantic-default-submodes
               'global-semantic-decoration-mode)
    (add-to-list 'semantic-default-submodes
               'global-semantic-idle-summary-mode)
    (add-to-list 'semantic-default-submodes
               'global-semantic-idle-local-symbol-highlight-mode)
    (add-to-list 'semantic-default-submodes
               'global-semantic-mru-bookmark-mode))
