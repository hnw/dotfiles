;; auto-save-buffers-enhanced: Emacsでファイルの自動保存
;; http://blog.kentarok.org/entry/20080222/1203688543
;;
;; ただし、TRAMPでssh接続している場合auto-saveと相性が悪すぎるので除外
;; また、auto-saveとhaskell-modeの相性が悪いので*.hsを除外
(when (require 'auto-save-buffers-enhanced nil t)
  (auto-save-buffers-enhanced t)
  (setq auto-save-buffers-enhanced-exclude-regexps
        '("^/sshx?:"
          "^/sudo:"
          "\\.hs$")))

;; また、auto-save-buffers-enhancedとhexl-modeの相性が悪いのでオフにする
(add-hook 'hexl-mode-hook
          (lambda ()
            (auto-save-buffers-enhanced nil)))
