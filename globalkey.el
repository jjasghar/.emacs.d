;; browse-url
(global-set-key (kbd "C-c b") 'browse-url-at-point)

;; dash-at-point
(global-set-key "\C-cd" 'dash-at-point)

;; run spell check on word
(global-set-key (kbd "M-s M-s") 'ispell-word)

;; comment region
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

;; indent-region
(global-set-key "\C-ci" 'indent-region)

;; text size
(global-set-key "\M-+" 'text-scale-increase)
(global-set-key "\M-_" 'text-scale-decrease)

;; ace-jump-mode
(global-set-key (kbd "C-0") 'ace-jump-char-mode)

;; write out a region i've marked
(global-set-key (kbd "C-x w") 'write-region)

;; google-this
(global-set-key (kbd "C-c M-g") 'google-this)

;; magit-status
(global-set-key (kbd "C-c g") 'magit-status)

;; shortcut for shell
(global-set-key (kbd "C-c s") 'shell)

;; buffer resize http://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; toggle horizontal and vertial splits
(global-set-key (kbd "C-x |") 'toggle-window-split)
