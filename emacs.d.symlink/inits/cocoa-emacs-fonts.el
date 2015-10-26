;; Cocoa Emacs
;; フォントセットを作る
(let* ((fontset-name "myfonts") ; フォントセットの名前
       (size my-font-size) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
       (asciifont "Menlo") ; ASCIIフォント
       (jpfont "Hiragino Maru Gothic ProN") ; 日本語フォント
       (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont))
       (fsn (create-fontset-from-ascii-font font nil fontset-name)))
  (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec) ; 半角カナ
  (set-fontset-font fsn '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
  (set-fontset-font fsn '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
  )
;; デフォルトのフレームパラメータでフォントセットを指定
(add-to-list 'default-frame-alist '(font . "fontset-myfonts"))
;; フォントサイズの比を設定
;;         (dolist (elt '(("^-apple-hiragino.*" . 1.15)
;;                        (".*osaka-bold.*" . 1.15)
;;                        (".*osaka-medium.*" . 1.15)
;;                        (".*courier-bold-.*-mac-roman" . 1.0)
;;                        (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
;;                        (".*monaco-bold-.*-mac-roman" . 0.9)))
;;           (add-to-list 'face-font-rescale-alist elt))
;; デフォルトフェイスにフォントセットを設定
;; # これは起動時に default-frame-alist に従ったフレームが
;; # 作成されない現象への対処
(set-face-font 'default "fontset-myfonts")
(setq-default line-spacing 0.15)
;; Cocoa Emacsで、全角記号が入れられなくなる問題を解消
(when (fboundp 'mac-add-key-passed-to-system)
  (mac-add-key-passed-to-system 'shift))
;; 以下、inline-patch が有効なときのための設定
;; Cocoa EmacsがIME状態を理解するように
(setq default-input-method "MacOSX")
;; 日本語入力をしていても、ミニバッファに入ると英語モードに切り替える
(when (fboundp 'mac-change-language-to-us)
  (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
  )
;; 日本語入力のオン／オフに応じてカーソルの色を変更
(setq default-frame-alist
      (append
       (list (cons 'cursor-color my-roman-cursor-color))
       default-frame-alist))
(when (fboundp 'mac-set-input-method-parameter)
  (dolist (japanese-input-source
           ;; ATOK対応
           ;;「*scratch*」バッファでIMを有効化した状態で「(mac-get-current-input-source)」を評価(C-j）で分かる
           '("com.justsystems.inputmethod.atok24.Japanese"
             "com.justsystems.inputmethod.atok25.Japanese"
             "com.justsystems.inputmethod.atok26.Japanese"
             ))
    (mac-set-input-method-parameter japanese-input-source 'title "あ")
    (mac-set-input-method-parameter japanese-input-source 'cursor-type 'box)
    (mac-set-input-method-parameter japanese-input-source 'cursor-color my-japanese-cursor-color)
    ))

;; 以下、Mac環境共通の設定
(setq mac-command-key-is-meta t)
(setq mac-option-modifier 'hyper)
;; CommandキーをMetaキーとして使う。
(setq mac-command-modifier 'meta)
(setq grep-find-use-xargs 'bsd)
(setq browse-url-generic-program "open")
;; Ctrl+SpaceをSpotLight＆ことえりに取られないようにする
(when (fboundp 'mac-add-ignore-shortcut)
  (mac-add-ignore-shortcut '(control ? )))
;; Emacs Mac Port 用設定
;; ミニバッファで入力する際に自動的にASCIIにする
(when (fboundp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1))
;; カーソルの色を変える
(when (fboundp 'mac-input-source)
  (defun my-mac-selected-keyboard-input-source-chage-function ()
    "英語のときはカーソルの色をdim grayに、日本語のときはbrownにします."
    (let ((mac-input-source (mac-input-source)))
      (set-cursor-color
       (if (string-match "\\.Roman$" mac-input-source)
           my-roman-cursor-color my-japanese-cursor-color))))
  (add-hook 'mac-selected-keyboard-input-source-change-hook
            'my-mac-selected-keyboard-input-source-chage-function))

