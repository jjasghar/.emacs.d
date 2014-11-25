;;
;; General Settings I like
;;

(setq user-mail-address "jj@getchef.com")
(setq user-full-name "JJ Asghar")

;; Turn off the useless toolbar
(tool-bar-mode -1)
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
(setq-default major-mode 'text-mode)
;; blink instead of beep
(setq visible-bell t)

;; show that damn whitespace
(setq show-trailing-whitespace t)

;; fixing up the scratch buffer
(setq initial-major-mode 'ruby-mode)
(setq initial-scratch-message "\
# This buffer is for notes you don't want to save, and for Ruby code.
# If you want to create a file, visit that file with C-x C-f,
# then enter the text in that file's own buffer.")

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

;; tack on flyspell to text
(add-hook 'text-mode-hook 'flyspell-mode)
(setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra"))

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

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; ;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(require 'ansi-color)

(load "~/.emacs.d/defined-aliases")
(load "~/.emacs.d/defined-fuctions")
(load "~/.emacs.d/erc")
(load "~/.emacs.d/globaladdhooks")
(load "~/.emacs.d/globalkey")
(load "~/.emacs.d/mac-emacs")
(load "~/.emacs.d/markdown-settings")
(load "~/.emacs.d/newsticker")
(load "~/.emacs.d/my-abbrevs")
(load "~/.emacs.d/my-flycheck")
(load "~/.emacs.d/my-gitgutter")
(load "~/.emacs.d/my-ido")
(load "~/.emacs.d/my-twit")
(load "~/.emacs.d/my-orgmode")
(load "~/.emacs.d/webjump")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" default)))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services smiley stamp spelling track)))
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(org-agenda-files (quote ("~/org/emacs.org")))
 '(send-mail-function nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
