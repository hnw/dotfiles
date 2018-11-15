;; settings for golang

;; see: http://emacs-jp.github.io/programming/golang.html

(add-hook 'go-mode-hook
          '(lambda()
             (setq tab-width 4)
             (setq indent-tabs-mode t)
             (local-set-key (kbd "M-.") 'godef-jump)
             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
             (local-set-key (kbd "C-c i") 'go-goto-imports)
             (local-set-key (kbd "C-c d") 'godoc)))

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook
          (lambda()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (local-set-key (kbd "M-.") 'godef-jump)
            (setq indent-tabs-mode nil)    ; タブを利用
            (setq c-basic-offset 4)        ; tabサイズを4にする
            (setq tab-width 4)))

;;; company-go
;;; via: https://github.com/emacsmirror/company-go

(require 'company-go)                                ; load company mode go backend

;;; Only use company-mode with company-go in go-mode

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

