;;; init.el

;; emacs -l init.el等で直接ロードしたときに, user-emacs-directoryが書き換わる
;; http://blog.shibayu36.org/entry/2015/05/01/172215
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; init-loaderがインストールされていなかったら
;; 明示的にinit-loader/20-package.elをload、init-loaderをインストールする
(unless (require 'init-loader nil t)
  (load-file (concat user-emacs-directory "inits/20-package.el"))
  (require 'init-loader)
)

;;; バイトコンパイルされていないelファイルの方が新しい場合そっちをロードする
(setq load-prefer-newer t)

;;; emacs起動時にエラーを報告する
;(setq init-loader-show-log-after-init 'error-only)

(init-loader-load (concat user-emacs-directory "inits"))

