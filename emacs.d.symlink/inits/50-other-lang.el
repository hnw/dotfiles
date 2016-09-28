;; settings for Perl
(add-hook 'perl-mode-hook
          (lambda ()
            (setq tab-width 4
                  indent-tabs-mode nil)))
(add-hook 'cperl-mode-hook
          (lambda ()
            (setq tab-width 4
                  indent-tabs-mode nil)))

;; settings for Elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

;; settings for Shell Script
(add-hook 'sh-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  sh-basic-offset 4
                  sh-indentation 4
                  ;; Tweak the indentation level of case-related syntax elements, to avoid
                  ;; excessive indentation because of the larger than default value of
                  ;; `sh-basic-offset' and other indentation options.
                  sh-indent-for-case-label 0
                  sh-indent-for-case-alt '+)))

;; settings for C
(add-hook 'c-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode t)))

;; settings for C++
(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

;; settings for C#
;; http://ongaeshi.hatenablog.com/entry/20110116/1295187496
(add-hook 'csharp-mode-hook
          '(lambda()
             (setq comment-column 40)
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil)

             ;; オフセットの調整
             (c-set-offset 'substatement-open '0)
             (c-set-offset 'case-label '+)
             (c-set-offset 'arglist-intro '+)
             (c-set-offset 'arglist-close '0)

             ;(font-lock-add-magic-number)
             )
          )

;; settings for Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; settings for OCaml
(if (require 'caml-font nil t)
    (progn
      (setq auto-mode-alist (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
      (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
      (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
      (autoload 'camldebug "camldebug" "camldebug" t)))

;; settings for Erlang
(when (require 'erlang-start nil t)
  (setq auto-mode-alist (cons '("\\.erl$" . erlang-mode) auto-mode-alist))
  (setq erlang-root-dir "/opt/local/lib/erlang"))
