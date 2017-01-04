(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/my-init.org"))
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
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first :search-type nil)
     (:name "all mail" :query "*" :key "a" :sort-order newest-first)
     (:name "today" :query "date:midnight..now" :sort-order newest-first))) t)
 '(org-agenda-files
   (quote
    ("/Users/jasghar/org/notes.org" "/Users/jasghar/org/blah_priorites_date_.org" "/Users/jasghar/org/caching_project.org" "/Users/jasghar/org/docker.org" "/Users/jasghar/org/emacs.org" "/Users/jasghar/org/freenode.org" "/Users/jasghar/org/gem.org" "/Users/jasghar/org/guenter.org" "/Users/jasghar/org/hanlon.org" "/Users/jasghar/org/index.org" "/Users/jasghar/org/ipxe.org" "/Users/jasghar/org/irc.org" "/Users/jasghar/org/kitchen-cluster.org" "/Users/jasghar/org/landing_page_meeting.org" "/Users/jasghar/org/maas.org" "/Users/jasghar/org/magit.org" "/Users/jasghar/org/notes.nov3014.org" "/Users/jasghar/org/openbay_openstack.org" "/Users/jasghar/org/openstack_documentation.org" "/Users/jasghar/org/openstack_priorities.20150108.org" "/Users/jasghar/org/pry.org" "/Users/jasghar/org/rfc-for-upstart.org" "/Users/jasghar/org/singlestack.org" "/Users/jasghar/org/software_licences.org" "/Users/jasghar/org/statusmeetings.org" "/Users/jasghar/org/test.org" "/Users/jasghar/org/test_bable.org")))
 '(package-selected-packages
   (quote
    (golden-ratio go-mode twittering-mode terraform-mode ag flycheck-rust rust-mode kanban avy-zap avy writegood-mode google-this ivy true swiper toml-mode org htmlize ox-reveal ox-twbs yaml-mode notmuch web-mode git-gutter zenburn-theme gist markdown-mode org-bullets yasnippet try solarized-theme magit-gh-pulls magit-gerrit helm guide-key flycheck company)))
 '(send-mail-function nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:inherit nil :background "RoyalBlue4")))))
(put 'narrow-to-region 'disabled nil)
