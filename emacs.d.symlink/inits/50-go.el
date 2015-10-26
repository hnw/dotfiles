;; settings for golang
;(require 'go-mode-load)
(add-hook 'go-mode-hook
          '(lambda()
             (setq tab-width 4)
             (setq indent-tabs-mode t)
             (local-set-key (kbd "M-.") 'godef-jump)
             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
             (local-set-key (kbd "C-c i") 'go-goto-imports)
             (local-set-key (kbd "C-c d") 'godoc)))

(let ((alt-gofmt (executable-find "goimports")))
  (if alt-gofmt (setq gofmt-command alt-gofmt)))
(add-hook 'before-save-hook 'gofmt-before-save)
