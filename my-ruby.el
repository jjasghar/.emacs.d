;; ruby files stolen from: howardabrams

(when (require 'ruby-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.rb$"          . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby"     . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'"      . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'"     . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'"   . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'"        . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'"      . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile\\'"    . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile\\'"      . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.thor\\'"      . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rabl\\'"      . ruby-mode))
  (add-to-list 'auto-mode-alist '("Thorfile\\'"     . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile\\'"  . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.jbuilder\\'"  . ruby-mode))
  (add-to-list 'auto-mode-alist '("Podfile\\'"      . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.podspec\\'"   . ruby-mode))
  (add-to-list 'auto-mode-alist '("Berksfile\\'"    . ruby-mode)))

(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

;; rubocop mode
(when (require 'rubocop nil t)
  (add-hook 'ruby-mode-hook 'rubocop-mode))
;; rubocop packages and gem install rubocop
