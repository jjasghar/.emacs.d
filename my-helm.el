;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; helm-M-x
(global-set-key "\M-x" 'helm-M-x)

;; show the helm kill ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; helm-buffer-list
(global-set-key (kbd "C-x b") 'helm-mini)

;; helm-find-files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; enable fuzzy matching
(setq helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t
      helm-split-window-in-side-p           t )

;; optional fuzzy matching for helm-M-x
(setq helm-M-x-fuzzy-match t)

;; helm-google-suggest
(global-set-key (kbd "C-c M-g") 'helm-google-suggest)
