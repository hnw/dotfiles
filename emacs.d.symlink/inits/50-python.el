;; $ pip3 install jedi autopep8 flake8 epc

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

;; Settings for autopep8, pyflakes
;; via: http://ksknw.hatenablog.com/entry/2016/05/07/171239
(add-hook 'python-mode-hook
          '(lambda ()
             (require 'py-autopep8)
             (setq py-autopep8-options '("--max-line-length=200"))
             (setq flycheck-flake8-maximum-line-length 200)
             (py-autopep8-enable-on-save)))

(add-hook 'python-mode-hook
          '(lambda ()
             (flymake-mode t)
             ;;errorやwarningを表示する
             (require 'flymake-python-pyflakes)
             (flymake-python-pyflakes-load)))
