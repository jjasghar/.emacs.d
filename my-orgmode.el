;; orgmode
(setq org-log-done 'time)
(setq org-default-notes-file (concat "~/org/notes.org"))
(define-key global-map "\C-ct" 'org-capture)
(setq org-export-backends (quote (ascii html icalendar latex md)))
(setq org-startup-indented t)

;; for https://github.com/rlister/org-present
;; M-x org-present
;; C-c C-q for quit (which will return you back to vanilla org-mode)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

;; fontify code in code blocks
(setq org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   ))

;; shortcut to notes.org
(global-set-key (kbd "C-c n")
                (lambda () (interactive) (find-file "~/org/notes.org")))
