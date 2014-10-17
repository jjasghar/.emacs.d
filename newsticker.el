;;
;; Stolen from http://home.thep.lu.se/~karlf/emacs.html
;;

;;
;; Newsticker!
;;
(global-set-key (kbd "C-c C-n") 'newsticker-show-news)
(setq
 ;; newsticker-heading-format "%t"
 ;; newsticker-item-format "%t"
 ;; newsticker-desc-format "%d\n%c"
 ;; newsticker-hide-old-items-in-newsticker-buffer t
 ;; newsticker-html-renderer 'w3m-region
 ;; newsticker-frontend 'newsticker-plainview
 ;; newsticker-use-full-width nil
 newsticker-retrieval-interval 0   ;don't fetch when I'm not reading RSS
 newsticker-automatically-mark-items-as-old nil
 newsticker-url-list '(
                       ("abandonia" "http://www.abandonia.com/en/rss.xml" nil nil nil)
                       ("slashdot" "http://rss.slashdot.org/Slashdot/slashdot" nil nil nil)
                       ("SMBC" "http://www.smbc-comics.com/rss.php" nil nil nil)
                       ("laserbrain" "http://laserbrainstudios.com/feed/" nil nil nil)
                       ("BBC World" "http://feeds.bbci.co.uk/news/world/rss.xml" nil nil nil)
                       ("stallman" "http://www.stallman.org/rss/rss.xml" nil nil nil)

                       ))
