
;;
;; erc
;;

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?

    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "j^2" :full-name "JJ Asghar")
      )))

(setq erc-autojoin-channels-alist '(("freenode.net" "#openstack-chef" "#chef" "#chef-hacking" "#c6h12o6" "#emacs" "#austindevops"
                                     "#dwarffortress" "#reddit-diabetes" "#reddit-diabetes-ops" "#openstack-operators")
                                    ("paraphysics.net" "#eztv")))
;; if you’d like to join the same channels on any server
;; (setq erc-autojoin-channels-alist '((".*" "#help" "#emacs")))


;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"

                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; a nice filler
;;[12:34] <user-one> the quick brown fox jumps over the lazy dog.  the quick
;;       + brown fox jumps over the lazy dog
(setq erc-timestamp-format "[%H:%M] ")
 (setq erc-fill-prefix "      + ")

;; fill width
(make-variable-buffer-local 'erc-fill-column)
 (add-hook 'window-configuration-change-hook
	   '(lambda ()
	      (save-excursion
	        (walk-windows
		 (lambda (w)
		   (let ((buffer (window-buffer w)))
		     (set-buffer buffer)
		     (when (eq major-mode 'erc-mode)
		       (setq erc-fill-column (- (window-width w) 2)))))))))

;; erc growl
(defvar growlnotify-command (executable-find "/usr/local/bin/growlnotify") "The path to growlnotify")

(defun growl (title message)
  "Shows a message through the growl notification system using
 `growlnotify-command` as the program."
  (cl-flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))) )
    (let* ((process (start-process "growlnotify" nil
                                   growlnotify-command
                                   (encfn title)
                                   "-a" "Emacs"
                                   "-n" "Emacs")))
      (process-send-string process (encfn message))
      (process-send-string process "\n")
      (process-send-eof process)))
  t)

(defun my-erc-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (growl
     (concat "ERC: name mentioned on: " (buffer-name (current-buffer)))
     message
     )))

(add-hook 'erc-text-matched-hook 'my-erc-hook)

(load "~/.ercpass")
;; an example:
;;(setq freenode-nickone-pass "mynickservpass1")
;;(setq freenode-nicktwo-pass "mynickservpass2")
;;(setq dalnet-pass "mynickservpass3")
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
    (setq erc-nickserv-passwords
          `((freenode     (("j^2" . ,freenode-nickone-pass)
                           ("nick-two" . ,freenode-nicktwo-pass)))
            (DALnet       (("nickname" . ,dalnet-pass)))))


;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e") 'erc-start-or-switch) ;; ERC
