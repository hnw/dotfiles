;; settings for JavaScript
(add-hook 'javascript-mode-hook
          (lambda ()
            (setq javascript-indent-level 2
                  indent-tabs-mode nil)))

(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2
                  indent-tabs-mode nil)))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2
                  indent-tabs-mode nil)))
