;;
;; General Settings I like
;;

(setq user-mail-address "jj@chef.io")
(setq user-full-name "JJ Asghar")

;; Turn off the useless toolbar
(tool-bar-mode -1)
;; Turn off the scroll bar
;;(scroll-bar-mode -1)
;; Show paren mode (http://www.emacswiki.org/emacs/ShowParenMode)
(show-paren-mode 1)
;; backup files because well backupfiles
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t
)
;; default to text mode
(setq-default major-mode 'org-mode)
;; blink instead of beep
(setq ring-bell-function 'ignore)
;; (setq visible-bell t)

;; show that damn whitespace
(setq show-trailing-whitespace t)

;; fixing up the scratch buffer
(setq initial-major-mode 'lisp-mode)
(setq initial-scratch-message "\
#
# Scratch Scratch Scratch
#
#
")

;; fix Warning(undo): Buffer Buffer list
(add-hook 'Buffer-menu-mode-hook 'buffer-disable-undo)

;; display date and time
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; highlight incremental search
(setq query-replace-highlight t)

;; make a larger buffer for M-x shell
(setq comint-buffer-maximum-size 10240)

;; add the column line mode
(setq column-number-mode t)

;; tack on flyspell to text
(add-hook 'text-mode-hook 'flyspell-mode)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                ruby-mode-hook
                python-mode-hook
                js-mode-hook
                chef-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))

;; Lorem-ipsum stuff
(autoload 'Lorem-ipsum-insert-paragraphs "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-sentences "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-list "lorem-ipsum" "" t)

;; convert from double space at the end of a sentance to single
(setq sentence-end-double-space nil)

;; auto centering
(setq
 scroll-conservatively 1000
 scroll-margin 0
 scroll-preserve-screen-position 1
 auto-window-vscroll nil)

(server-start) ;; so it's listening for the emacsclient alias
(setq ns-pop-up-frames nil) ;; keep OSX from opening more windows
;; 'y' instead of 'yes'
(fset 'yes-or-no-p 'y-or-n-p)
;; always use spaces, never tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 2) ; set the tab width to two
;; skip startup message
(setq inhibit-startup-message t)

;; Add MELPA for packages --> http://melpa.org
(require 'package)

;; here there's a variable named package-archives, and we are adding the MELPA repository to it
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; loads packages and activates them
(package-initialize)

;; Converted to the solarized-dark theme
;; I also like the wombat,wheatgrass, but lets start here
;; (load-theme 'solarized-dark t)

;; introduced to zenburn (11/25/2014) I think i like it more
(load-theme 'zenburn t)

;; introduced to Spolsky (06/15/2015)
;; (load-theme 'spolsky t)

;; powerline because it's pretty
(require 'powerline)
(powerline-center-theme)

;; i need tab complete to work in ansi-term
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

;; i liked the idea of different highlighted color
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:inherit nil :background "RoyalBlue4")))))


;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 140
                    :weight 'normal
                    :width 'normal)

(require 'ansi-color)

;; eldoc mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; turn on company-mode
(add-hook 'after-init-hook 'global-company-mode)

(load "~/.emacs.d/defined-aliases")
(load "~/.emacs.d/globaladdhooks")
(load "~/.emacs.d/newsticker")
(load "~/.emacs.d/my-abbrevs")
(load "~/.emacs.d/my-defined-fuctions")
(org-babel-load-file "~/.emacs.d/my-dired.org")
;; (org-babel-load-file "~/.emacs.d/my-erc.org"
(org-babel-load-file "~/.emacs.d/my-flycheck.org")
(org-babel-load-file "~/.emacs.d/my-gitgutter.org")
(org-babel-load-file "~/.emacs.d/my-globalkey.org")
(org-babel-load-file "~/.emacs.d/my-gnus.org")
(load "~/.emacs.d/my-guide-key")
(org-babel-load-file "~/.emacs.d/my-helm.org")
(org-babel-load-file "~/.emacs.d/my-javascript.org")
(org-babel-load-file "~/.emacs.d/my-mac-emacs-settings.org")
(org-babel-load-file "~/.emacs.d/my-magit.org")
(org-babel-load-file "~/.emacs.d/my-markdown-settings.org")
(org-babel-load-file "~/.emacs.d/my-ruby.org")
(load "~/.emacs.d/my-twit")
(org-babel-load-file "~/.emacs.d/my-orgmode.org")
(org-babel-load-file "~/.emacs.d/my-yas.org")
(org-babel-load-file "~/.emacs.d/my-webmode.org")
(load "~/.emacs.d/webjump")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" default)))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services smiley stamp spelling track)))
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(org-agenda-files
   (quote
    ("/Users/jasghar/org/notes.org" "/Users/jasghar/org/blah_priorites_date_.org" "/Users/jasghar/org/caching_project.org" "/Users/jasghar/org/docker.org" "/Users/jasghar/org/emacs.org" "/Users/jasghar/org/freenode.org" "/Users/jasghar/org/gem.org" "/Users/jasghar/org/guenter.org" "/Users/jasghar/org/hanlon.org" "/Users/jasghar/org/index.org" "/Users/jasghar/org/ipxe.org" "/Users/jasghar/org/irc.org" "/Users/jasghar/org/kitchen-cluster.org" "/Users/jasghar/org/landing_page_meeting.org" "/Users/jasghar/org/maas.org" "/Users/jasghar/org/magit.org" "/Users/jasghar/org/notes.nov3014.org" "/Users/jasghar/org/openbay_openstack.org" "/Users/jasghar/org/openstack_documentation.org" "/Users/jasghar/org/openstack_priorities.20150108.org" "/Users/jasghar/org/pry.org" "/Users/jasghar/org/rfc-for-upstart.org" "/Users/jasghar/org/singlestack.org" "/Users/jasghar/org/software_licences.org" "/Users/jasghar/org/statusmeetings.org" "/Users/jasghar/org/test.org" "/Users/jasghar/org/test_bable.org")))
 '(package-selected-packages
   (quote
    (web-mode git-gutter zenburn-theme gist markdown-mode markdown-mode+ org-bullets yasnippet try solarized-theme powerline magit-gh-pulls magit-gerrit helm guide-key flycheck company)))
 '(send-mail-function nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(setq-default flycheck-disabled-checkers '(chef-foodcritic))

(add-to-list 'default-frame-alist '(font . "-outline-Monaco-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1" ))
(set-face-attribute 'default t :font "-outline-Monaco-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1" )
