;;; key bindings

(define-key global-map "\C-h" 'delete-backward-char)

(global-set-key "\M-n" 'browse-url-at-point)
(global-set-key "\M-2" 'make-frame)
(global-set-key "\M-0" 'delete-frame)

;; C-RETで全画面表示(Cocoa Emacs用)
(global-set-key "\C-\M-m" 'ns-toggle-fullscreen)
