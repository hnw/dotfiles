(require 'cl-lib)

;; encoding
(set-language-environment "Japanese")
;; euc-jp, utf-8, shift_jis, iso-2022-jp, その他の順で自動判定
(prefer-coding-system 'iso-2022-jp)
(prefer-coding-system 'shift_jis)
(prefer-coding-system 'utf-8)
(prefer-coding-system 'euc-jp)
(cond ((boundp 'buffer-file-coding-system)
       (setq buffer-file-coding-system 'utf-8))
      (t
       (setq default-buffer-file-coding-system 'utf-8)))
(set-default-coding-systems 'utf-8)

;(setq-default horizontal-scroll-bar nil)

;; for GC
(setq-default gc-cons-threshold (* gc-cons-threshold 10))

;; echo stroke
(setq-default echo-keystrokes 0.1)

;; 以前開いたファイルを再度開いたとき、元のカーソル位置を復元する
;; http://www.emacswiki.org/emacs/SavePlace

(when (require 'saveplace nil t)
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "saved-places")))

(savehist-mode t)
;(save-place-mode t)

;; info for japanese
(auto-compression-mode t)

;; highlight mark region
(transient-mark-mode t)

;; indicate last line
(setq-default indicate-empty-lines t
              indicate-buffer-boundaries 'right)

;; Disable default scroll bar and tool bar
(when window-system
;  (set-scroll-bar-mode 'nil)
  (tool-bar-mode 0))

;; Disable menu bar
(menu-bar-mode -1)

;; not beep
(setq-default ring-bell-function 'ignore)

;; display line infomation
(line-number-mode t)

;; paren-mode
(show-paren-mode t)

;; history
(setq-default history-length 500
              history-delete-duplicates t)

;; run server
(require 'server)
(unless (server-running-p)
  (server-start))

;; which-func
(which-function-mode t)
(setq-default which-func-unknown "")

;; invisible mouse cursor when editing text
(setq-default make-pointer-invisible t)

;; undo setting
(setq-default undo-no-redo t
              undo-limit 600000
              undo-strong-limit 900000)
