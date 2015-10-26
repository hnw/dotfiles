;; settings for HTML
(add-hook 'html-mode-hook
          (lambda ()
            (setq tab-width 2
                  indent-tabs-mode nil)))

;; html-modeの、セーブ時にファイルの文字コードをHTMLのMETAタグに合わせて変更する機能をオフにする
(if (boundp 'auto-coding-functions)
    (delete 'sgml-html-meta-auto-coding-function auto-coding-functions))
