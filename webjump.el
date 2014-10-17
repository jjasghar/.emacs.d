;; bind webjump to C-x g http://www.neilvandyke.org/webjump/webjump.el
(global-set-key (kbd "C-x g") 'webjump)
(setq webjump-sites
         (append '(
                   ("Chef Docs"           . "docs.getchef.com/search.html")
                   ("Geektools Whois"     .
                    [simple-query "www.geektools.com/whois.html"
                                  "www.geektools.com/geektools-cgi/whois.cgi?query=" ""])
                   ("Github"              .
                    [simple-query "www.github.com" "https://github.com/search?utf8=✓&q=" ""])
                   ("Google"              .
                    [simple-query "www.google.com" "www.google.com/search?q=" ""])
                   ("Google Groups"       .
                    [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
                   ("IMDB"               .
                    [simple-query "www.imdb.com"  "www.imdb.com/find?q=" ""])
                   ("Twitter"            . "www.twitter.com/")
                   ("Wikipedia"          .
                    [simple-query "en.wikipedia.org" "en.wikipedia.org/w/index.php?search=" ""])
                   )))
