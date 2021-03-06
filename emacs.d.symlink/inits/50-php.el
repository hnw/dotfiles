;; settings for PHP
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

;; *.phpをphp-modeで開くように （念のため）
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; *.twigをhtml-modeで開くように
(add-to-list 'auto-mode-alist '("\\.twig$" . html-mode))

(eval-after-load 'php-mode
  '(progn
     (setq local-php-manual-path
           "/usr/lib/php/data/phpman/php-chunked-xhtml/index.html")
     (add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
     (add-hook 'php-mode-hook
               (lambda ()
                 ;; camelCaseなシンボルの単語区切りを認識して移動しやすくする
                 ;; https://github.com/ejmr/php-mode#subword-mode
                 (subword-mode 1)
                 ;; コーディングスタイルの設定。
                 ;; M-x customize-group <RET> phpでカスタマイズ可能。
                 ;(php-enable-default-coding-style)
                 ;; C-c RET: php-browse-manual
                 (if (file-readable-p local-php-manual-path)
                     (setq php-manual-url (concat "file://" local-php-manual-path))
                   (setq php-manual-url "http://www.php.net/manual/ja/"))
                 ;; C-c C-f: php-search-documentation
                 (setq php-search-url "http://jp2.php.net/")
                 ;; 関数の先頭・最後に移動
                 ;;(define-key php-mode-map "\C-\M-a" 'php-beginning-of-defun)
                 ;;(define-key php-mode-map "\C-\M-e" 'php-end-of-defun)
                 ;; indent
                 (setq tab-width 4
                       indent-tabs-mode nil)))
     ;; php-modeで関数名を補完
     ;; C-M-iで利用中。
     ;; 辞書の作り方については下記URLを参照。
     ;; http://www.asahi-net.or.jp/~wv7y-kmr/memo/vim_php.html#fromPHPScript
     (setq my-php-completion-file
           (expand-file-name "~/lib/emacs/php/php-completion-file"))
     (if (file-readable-p my-php-completion-file)
         (add-hook 'php-mode-hook
                   '(lambda ()
                      (setq php-completion-file my-php-completion-file)
                      ;; M-TAB が有効にならないので以下の設定を追加
                      (define-key php-mode-map "\C-\M-i" 'php-complete-function))))
     ))

;; flymake (Emacs22から標準添付されている)
(when (locate-library "flymake")
  ;; PHP用設定
  (add-hook 'php-mode-hook
            '(lambda()
               (flymake-mode t)
               (when (not (fboundp 'flymake-php-init))
                 ;; flymake-php-initが未定義のバージョンだったら、自分で定義する
                 (defun flymake-php-init ()
                   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                                        'flymake-create-temp-inplace))
                          (local-file  (file-relative-name
                                        temp-file
                                        (file-name-directory buffer-file-name))))
                     (list "php" (list "-f" local-file "-l")))))
               (setq flymake-allowed-file-name-masks
                     (append
                      flymake-allowed-file-name-masks
                      '(("\\.php[345]?$" flymake-php-init))))
               (setq flymake-err-line-patterns
                     (cons
                      '("\\(\\(?:Parse error\\|Fatal error\\|Warning\\): .*\\) in \\(.*\\) on line \\([0-9]+\\)" 2 3 nil 1)
                      flymake-err-line-patterns))
               )))

;; company-php
;; https://github.com/xcwen/ac-php
;; via: https://qiita.com/nanasess/items/b5dc322bac34107cf067
(add-hook 'php-mode-hook
          '(lambda ()
             (require 'company-php)
             (company-mode t)
             (ac-php-core-eldoc-setup) ;; enable eldoc
             (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'company-ac-php-backend)))
