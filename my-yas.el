;; enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"   ;; personal snippets
        "~/.emacs.d/elpa/yasnippet-20141223.303/snippets" ;; melpa install
        ))

(add-to-list 'load-path
             "~/.emacs.d/elpa/yasnippet-20141223.303/snippets")

(define-minor-mode chef-mode
  "This is to activate the chef-mode for yasnippets"
  :lighter " chef-mode")

;; When entering ruby-mode, consider also the snippets in the
;; snippet table "chef-mode"
(add-hook 'ruby-mode-hook
          #'(lambda ()
              (yas-activate-extra-mode 'chef-mode)))
