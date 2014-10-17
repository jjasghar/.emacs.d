;; orgmode
(setq org-log-done 'time)
(setq org-default-notes-file (concat "~/org/notes.org"))
(define-key global-map "\C-ct" 'org-capture)
(setq org-export-backends (quote (ascii html icalendar latex md)))
(setq org-startup-indented t)
