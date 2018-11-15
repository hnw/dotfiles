;; company-php
;; https://github.com/xcwen/ac-php
;; via: https://qiita.com/nanasess/items/b5dc322bac34107cf067
(add-hook 'python-mode-hook
          '(lambda ()
             (require 'company-jedi)
             (company-mode t)
             (jedi:setup)
             (setq jedi:complete-on-dot t)
             (setq jedi:use-shortcuts t)
             (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'company-jedi)))
