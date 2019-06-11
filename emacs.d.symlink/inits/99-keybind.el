;;; key bindings

;; term/bobcatの代わり
(keyboard-translate ?\C-h 'backspace)

(global-set-key "\M-n" 'browse-url-at-point)
(global-set-key "\M-2" 'make-frame)
(global-set-key "\M-0" 'delete-frame)

;; C-RETで全画面表示(Cocoa Emacs用)
(global-set-key "\C-\M-m" 'ns-toggle-fullscreen)

;; yen markの代わりにバックスラッシュを入力する
;; via: https://nekotank.hateblo.jp/entry/20130628/1372411052
(define-key global-map [?¥] [?\\])
