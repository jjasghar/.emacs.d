;; orgmode
(setq org-log-done 'time)
(setq org-default-notes-file (concat "~/org/todo.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-export-coding-system 'utf-8)
(setq org-export-backends (quote (ascii html icalendar latex md)))
(setq org-startup-indented t)

;; org-capture templates
(setq org-capture-templates
      '(("t"              ; hotkey
         "TODO list item" ; name
         entry            ; type
         ; heading type and title
         (file+headline org-default-notes-file "Uncategorized TODOs")
         "*** TODO %? \n %i Captured at %U \n %i %a") ; template
        ("j"
         "Journal entry"
         entry
         (file+datetree "~/org/journal.org")
         (file "~/.emacs.d/org-templates/journal.orgcaptmpl"))
        ("b"
         "Tidbit: quote, zinger, one-liner or textlet"
         entry
         (file+headline org-default-notes-file "tidbits")
         (file "~/.emacs.d/org-templates/tidbit.orgcaptmpl"))
        ("l"
         "Link to look up later"
         entry
         (file+headline org-default-notes-file "links")
         (file "~/.emacs.d/org-templates/links.orgcaptmpl"))
        ))

(setq org-todo-keywords
      '((sequence "TODO(t)" "BLOCKED(b)" "INPROGRESS(i)" "|" "DONE(d)")
        (sequence "KNOWNISSUE(k)" "|" "CANCELED(c)")
        ))


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
(global-set-key (kbd "C-c t")
                (lambda () (interactive) (find-file "~/org/todo.org")))

(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                ))
  (select-frame-by-name "remember")
  (org-capture))
