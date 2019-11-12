
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(org-babel-load-file "/home/jj/.emacs.d/my-init.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(package-selected-packages
   (quote
    (ox-twbs yasnippet-snippets yaml-mode writegood-mode web-mode use-package toml-mode terraform-mode swiper rust-mode restclient powershell org-re-reveal org-present markdown-mode kubernetes-tramp kubernetes js2-mode helm guide-key-tip groovy-mode go-mode git-link gist flycheck-rust ecukes dokuwiki-mode dockerfile-mode company avy atomic-chrome)))
 '(send-mail-function nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:inherit nil :background "RoyalBlue4")))))
