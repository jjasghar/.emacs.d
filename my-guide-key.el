(require 'guide-key)
;; add the keys you'd like guide-key to suggest
(setq guide-key/guide-key-sequence '("C-x r"
                                     "C-x 4"
                                     "C-c"
                                     "C-c C-x"
                                     ))
(guide-key-mode 1)  ; Enable guide-key-mode
