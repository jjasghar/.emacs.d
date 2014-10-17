;; git-gutter mode
(add-hook 'ruby-mode-hook 'git-gutter-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(send-mail-function nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
