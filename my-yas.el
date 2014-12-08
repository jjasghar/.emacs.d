
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"   ;; personal snippets
        "~/.emacs.d/elpa/yasnippet-20141117.327/snippets" ;; melpa install
        ))


;; When entering ruby-major-mode, consider also the snippets in the
;; snippet table "chef-mode"
(add-hook 'ruby-mode-hook
          #'(lambda ()
              (yas-activate-extra-mode 'chef-mode)))


;; enable yasnippet
(yas-global-mode 1)
