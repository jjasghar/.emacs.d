;; orgmode
(setq org-log-done 'time)
(setq org-default-notes-file (concat "~/org/notes.org"))
(define-key global-map "\C-ct" 'org-capture)
(setq org-export-backends (quote (ascii html icalendar latex md)))
(setq org-startup-indented t)

(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)))

(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)))

;; fontify code in code blocks
(setq org-src-fontify-natively t)
