;; I like two buffers but sometimes i like it vert/horizontal
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; Remove ^M------------------------------------
;; Call with M-x strip[TAB]
(defun strip-^m ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil nil)
    (replace-match "")))
;;(define-key esc-map "o" 'strip-^m)
;;--------------------------------------------

;; zones http://www.emacswiki.org/emacs/ZoneMode
(defun zone-choose (pgm)
   "Choose a PGM to run for `zone'."
   (interactive
    (list
     (completing-read
      "Program: "
      (mapcar 'symbol-name zone-programs))))
   (let ((zone-programs (list (intern pgm))))
     (zone)))


;; my first fuction in emacs :)
(defun reddit-emacs ()
  "Opens up reddit-emacs"
  (interactive)
  (browse-url "http://reddit.com/r/emacs"))
(global-set-key (kbd "C-c C-r") 'reddit-emacs)


;; transpose buffers http://www.emacswiki.org/emacs/TransposeWindows
(defun flip-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))


;; only turn on line numbers when GoTo line is on
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))


;; Kill all other buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; create scratch buffer
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun insert-epoch () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%s)")))
(defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun my/insert-line-before (times)
  "insterts a newline above the line containing
   the cursor."
  (interactive "p")
  (save-excursion
    (move-beginning-of-line 1)
    (newline times)))

(global-set-key (kbd "C-S-o")
                'my/insert-line-before)

;; the-the fuction -> http://www.gnu.org/software/emacs/manual/html_mono/eintr.html#the_002dthe
(defun the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))

;; added a binding for require pry
(defun insert-require-pry-binding-pry ()
  "inserts require pry; binding.pry."
       (interactive)
       (insert "require 'pry'; binding.pry"))

(global-set-key "\C-c\C-p" 'insert-require-pry-binding-pry)
