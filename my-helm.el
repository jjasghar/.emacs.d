;; helm-M-x
(global-set-key "\M-x" 'helm-M-x)

;; optional fuzzy matching for helm-M-x
(setq helm-M-x-fuzzy-match t)

;; show the helm kill ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; helm-buffer-list
(global-set-key (kbd "C-x b") 'helm-mini)

;; enable fuzzy matching
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
