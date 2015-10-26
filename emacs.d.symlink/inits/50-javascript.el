;; settings for JavaScript
(add-hook 'javascript-mode-hook
          (lambda ()
            (setq tab-width 4
                  indent-tabs-mode nil)
            (setq javascript-indent-level 4)
            (setq javascript-basic-offset tab-width)))
