;; Carbon Emacs
(when (require 'carbon-font nil t)
  (fixed-width-set-fontset "osaka" my-font-size)
  (setq fixed-width-rescale nil)
  ;; 日本語入力のオン／オフに応じてカーソルの色を変更
  (mac-set-input-method-parameter
                  'roman 'cursor-color my-roman-cursor-color)
  (mac-set-input-method-parameter
                  'japanese 'cursor-color my-japanese-cursor-color))

;; 以下、Mac環境共通の設定
(setq mac-command-key-is-meta t)
(setq mac-option-modifier 'hyper)
;; CommandキーをMetaキーとして使う。
(setq mac-command-modifier 'meta)
(setq grep-find-use-xargs 'bsd)
(setq browse-url-generic-program "open")
;; Ctrl+SpaceをSpotLight＆ことえりに取られないようにする
(mac-add-ignore-shortcut '(control ? ))
