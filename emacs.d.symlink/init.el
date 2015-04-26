;; .emacs

;; http://www.sodan.org/~knagano/emacs/dotemacs.html
;; （バイトコンパイル時のwarningを抑止する目的に使っている）
(defmacro exec-if-bound (sexplist)
  "関数が存在する時だけ実行する（car の fboundp を調べるだけ）"
  `(if (fboundp (car ',sexplist))
       ,sexplist))

;; add own directory to load-path

(setq load-path
      (append
       (list
        (expand-file-name "~/lib/emacs")
        (expand-file-name "~/lib/emacs/org-7.7/lisp")
        (expand-file-name "~/share/emacs/site-lisp")
        "/usr/local/share/emacs/site-lisp"
        "/opt/local/lib/erlang/lib/tools-2.6.1/emacs/"
        )
       load-path))

;; package.el
;; http://qiita.com/catatsuy/items/5f1cd86e2522fd3384a0
;; http://www.robario.com/2013/08/07

(defvar my/favorite-packages
  '(
    php-mode
    haskell-mode
    csharp-mode
    yaml-mode
    go-mode
    open-junk-file
    gtags
    anything
    auto-async-byte-compile
    auto-save-buffers-enhanced
    )
  "起動時に自動的にインストールされるパッケージのリスト")

(eval-when-compile
  (require 'cl))

(when (require 'package nil t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize)
  (let ((pkgs (loop for pkg in my/favorite-packages
                    unless (exec-if-bound (package-installed-p pkg))
                    collect pkg)))
    (when pkgs
      ;; check for new packages (package versions)
      (message "%s" "Get latest versions of all packages...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (pkg pkgs)
        (package-install pkg)))))

;; init.elcよりinit.elの方が新しかったら再度byte-compileする
;; http://emacswiki.org/emacs/AutoRecompile
(defun byte-compile-user-init-file-if-needed ()
  (interactive)
  (when (and user-init-file
             (equal (file-name-extension user-init-file) "elc"))
    (let* ((source (file-name-sans-extension user-init-file))
           (alt (concat source ".el")))
      (setq source (cond ((file-exists-p alt) alt)
                         ((file-exists-p source) source)
                         (t nil)))
      (when (and source
                 (file-newer-than-file-p source user-init-file))
        (byte-compile-file source)
        (load-file source)
	(eval-buffer nil nil)
        (delete-other-windows)))))

(byte-compile-user-init-file-if-needed)

(when (require 'auto-async-byte-compile nil t)
  (setq auto-async-byte-compile-init-file "~/.emacs.d/async-compile-init.el")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))


;;; 文字コード関連の共通設定
;; 日本語をデフォルトにする。
(set-language-environment "Japanese")

;; euc-jp, utf-8, shift_jis, iso-2022-jp, その他の順で自動判定
(prefer-coding-system 'iso-2022-jp)
(prefer-coding-system 'shift_jis)
(prefer-coding-system 'utf-8)
(prefer-coding-system 'euc-jp)
;;(set-terminal-coding-system 'utf-8)
;;(setq file-name-coding-system 'utf-8)
;;(set-clipboard-coding-system 'utf-8)
(cond ((boundp 'buffer-file-coding-system)
       (setq buffer-file-coding-system 'utf-8))
      (t
       (setq default-buffer-file-coding-system 'utf-8)))

;;(setq coding-system-for-read 'utf-8-unix)
(set-default-coding-systems 'utf-8)
;;(set-keyboard-coding-system 'utf-8)
;;(set-buffer-file-coding-system 'euc-jp)


;;; フォントサイズ・フレームサイズの設定
(when window-system
  ;; フォントサイズ・フレームサイズを定義する連想配列
  ;; (解像度 フォントサイズ フレーム幅 フレーム高)で定義
  ;; 解像度について、マルチディスプレイ環境ではEmacsの実装ごとに取れる値が変わる模様。
  ;; 適宜なんとかしてください。
  (setq my-resolution-alist
        '(
          ("1440x900@24" 16  nil 37)   ; 16pt,81列37行(実質32行), MBA13@CocoaEmacs
          ;("1440x900@24" 24  nil 32)   ; 24pt,81列32行(実質27行), MBA13@CocoaEmacs
          ("1366x768@24" 16  nil 37)   ; 16pt,81列37行(実質34行), MBA11@CocoaEmacs
          ("1366x768@23" 16  nil 39)   ; 16pt,81列39行(実質34行), MBA11@CocoaEmacs
          ;("1366x768@23" 24  nil 32)   ; 24pt,81列32行, MBA11@CocoaEmacs, プレゼン用
          ("1366x768@22" 16  nil 34)   ; 16pt,81列34行, MBA11@CarbonEmacs
          ("1280x800@22" nil nil 45)   ; 14pt,81列45行, MBP@CarbonEmacs
          (nil           14  81  59))) ; 14pt,81列59行, デフォルト設定

  ;; 解像度+Emacsバージョン, 例:1366x768@23
  (setq main-display-resolution
        (format "%dx%d@%d"
                (x-display-pixel-width)
                (x-display-pixel-height)
                emacs-major-version))
  (defvar my-font-size nil)
  (let* ((list-merge
          (lambda (list1 list2)
            (if (and (not list1) (not list2))
                nil
              (cons (or (car list1) (car list2))
                    (funcall list-merge (cdr list1) (cdr list2))))))
         (resolution-list (funcall list-merge
                                   (cdr (assoc main-display-resolution my-resolution-alist))
                                   (cdr (assoc nil my-resolution-alist))))
         (frame-width-alist (if (nth 1 resolution-list)
                                (list (cons 'width (nth 1 resolution-list)))
                              nil))
         (frame-height-alist (if (nth 2 resolution-list)
                                 (list (cons 'height (nth 2 resolution-list)))
                               nil))
         (frame-size-alist (append frame-width-alist
                                   frame-height-alist)))
    ;; フォントサイズの設定
    (setq my-font-size (car resolution-list))
    ;; 初期フレームの位置設定
    (setq initial-frame-alist
          (append
           '((top  . 26)    ; フレームの Y 位置(ピクセル数)
             (left . 30))   ; フレームの X 位置(ピクセル数)
           initial-frame-alist))
    ;; フレームのサイズ設定
    (setq default-frame-alist
          (append
           frame-size-alist
           default-frame-alist))
))
;; カーソルの色の設定。普段は"dim gray"、日本語のときは"brown"
(setq my-roman-cursor-color "dim gray")
(setq my-japanese-cursor-color "brown")

;;;フォント設定
(when (and (eq system-type 'darwin) window-system)
  (cond ((eq emacs-major-version 22)
         ;; Carbon Emacs
         (require 'carbon-font)
         (exec-if-bound (fixed-width-set-fontset "osaka" my-font-size))
         (setq fixed-width-rescale nil)
         ;; 日本語入力のオン／オフに応じてカーソルの色を変更
         (exec-if-bound (mac-set-input-method-parameter
                         'roman 'cursor-color my-roman-cursor-color))
         (exec-if-bound (mac-set-input-method-parameter
                         'japanese 'cursor-color my-japanese-cursor-color)))
        ((>= emacs-major-version 23)
         ;; Cocoa Emacs
         ;; フォントセットを作る
         (let* ((fontset-name "myfonts") ; フォントセットの名前
                (size my-font-size) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
                (asciifont "Menlo") ; ASCIIフォント
                (jpfont "Hiragino Maru Gothic ProN") ; 日本語フォント
                (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
                (fontspec (exec-if-bound (font-spec :family asciifont)))
                (jp-fontspec (exec-if-bound (font-spec :family jpfont)))
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
;         (dolist (elt '(("^-apple-hiragino.*" . 1.15)
;                        (".*osaka-bold.*" . 1.15)
;                        (".*osaka-medium.*" . 1.15)
;                        (".*courier-bold-.*-mac-roman" . 1.0)
;                        (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
;                        (".*monaco-bold-.*-mac-roman" . 0.9)))
;           (add-to-list 'face-font-rescale-alist elt))
         ;; デフォルトフェイスにフォントセットを設定
         ;; # これは起動時に default-frame-alist に従ったフレームが
         ;; # 作成されない現象への対処
         (set-face-font 'default "fontset-myfonts")
         (setq-default line-spacing 0.15)
         ;; Cocoa Emacsで、全角記号が入れられなくなる問題を解消
         (exec-if-bound (mac-add-key-passed-to-system 'shift))
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
         ))
  ;; 以下、Mac環境共通の設定
  (setq mac-command-key-is-meta t)
  (setq mac-option-modifier 'hyper)
  ;; CommandキーをMetaキーとして使う。
  (setq mac-command-modifier 'meta)
  (setq grep-find-use-xargs 'bsd)
  (setq browse-url-generic-program "open")
  ;; Ctrl+SpaceをSpotLight＆ことえりに取られないようにする
  (exec-if-bound (mac-add-ignore-shortcut '(control ? )))
  )

;; hide menubar/toolbar
(exec-if-bound (menu-bar-mode 0))
(exec-if-bound (tool-bar-mode 0))

;; http://sakito.jp/emacs/emacsshell.html#path
;; 後に記述したものの方が PATH の先頭に追加されます
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/usr/local/sbin"
              "/usr/local/bin"
              (expand-file-name "~/bin")
              ))
  ;; PATH と exec-path に同じ内容を追加します
  (when ;; (and
         (file-exists-p dir) ;; (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;; Avoid running silly /usr/share/emacs/site-lisp/default.el.
;; (for Fedora/RHEL/CentOS)
(setq inhibit-default-init t)

(when window-system
  ;; enable wheelmouse support by default
  (mwheel-install)
  ;; make switching frames works properly under the default click-to-focus
  (setq focus-follows-mouse nil))

;; turn on font-lock mode
(exec-if-bound (global-font-lock-mode t))

;; enable visual feedback on selections
;;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; Always end a file with a newline
(setq require-final-newline t)

;; avoid "Symbolic link to SVN-controlled source file; follow link? (yes or no)"
(setq vc-follow-symlinks t)

;; anthy.el をロードする。
(if (require 'anthy nil t)
    ;; japanese-anthy をデフォルトの input-method にする。
    (setq default-input-method "japanese-anthy"))

;; スクリプトっぽかったら勝手に実行ビットを立てる
;; http://www.emacswiki.org/emacs/MakingScriptsExecutableOnSave
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;;
;;; Org mode
;;;
(when (require 'org-install nil t)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb))

;; settings for PHP
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(setq local-php-manual-path
      "/usr/lib/php/data/phpman/php-chunked-xhtml/index.html")
(add-hook 'php-mode-hook
          (lambda ()
            (when (require 'cc-subword nil t)
              (exec-if-bound (c-subword-mode 1)))
            (c-set-style hnw/default-php-indentation-style)
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

;; *.twigをhtml-modeで開くように
(setq auto-mode-alist (cons '("\\.twig$" . html-mode) auto-mode-alist))

; (c-basic-offset . 2) はしばらく封印
(c-add-style "php/symfony"
             '((c-basic-offset . 4)
               (c-offsets-alist . (
                                   (defun-open            . 0)
                                   (defun-close           . 0)
                                   (defun-block-intro     . +)
                                   (topmost-intro         . 0)
                                   (topmost-intro-cont    . c-lineup-topmost-intro-cont)
                                   (block-open            . 0)
                                   (block-close           . 0)
                                   (statement             . 0)
                                   (statement-cont        . +)
                                   (statement-block-intro . +)
                                   (statement-case-intro  . +)
                                   (statement-case-open   . 0)
                                   (substatement          . +)
                                   (substatement-open     . 0)
                                   (case-label            . +)
                                   (comment-intro         . (c-lineup-knr-region-comment c-lineup-comment))
                                   (arglist-intro         . +)
                                   (arglist-cont          . (c-lineup-gcc-asm-reg 0))
                                   (arglist-cont-nonempty . +)
                                   (arglist-close         . 0)
                                   ))))

(c-add-style "php/zend"
             '((c-basic-offset . 4)
               (c-offsets-alist . (
                                   (defun-open            . 0)
                                   (defun-close           . 0)
                                   (defun-block-intro     . +)
                                   (topmost-intro         . 0)
                                   (topmost-intro-cont    . c-lineup-topmost-intro-cont)
                                   (block-open            . 0)
                                   (block-close           . 0)
                                   (statement             . 0)
                                   (statement-cont        . +)
                                   (statement-block-intro . +)
                                   (statement-case-intro  . +)
                                   (statement-case-open   . +)
                                   (substatement          . +)
                                   (substatement-open     . 0)
                                   (case-label            . +)
                                   (arglist-intro         . +)
                                   (arglist-cont          . (c-lineup-gcc-asm-reg 0))
                                   (arglist-cont-nonempty . +)
                                   (arglist-close         . 0)
                                   ))))

(defcustom hnw/default-php-indentation-style "php/symfony"
  "PHP indentation style."
  :type '(choice (string :tag "Symfony" "php/symfony")
                 (string :tag "Zend Framework" "php/zend"))
  :group 'hnw)

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

;; settings for HTML
(add-hook 'html-mode-hook
          (lambda ()
            (setq tab-width 2
                  indent-tabs-mode nil)))

;; settings for JavaScript
(add-hook 'javascript-mode-hook
          (lambda ()
            (setq tab-width 4
                  indent-tabs-mode nil)
            (setq javascript-indent-level 4)
            (setq javascript-basic-offset tab-width)))

;; settings for YAML
;;   yaml-mode は下記を利用。
;;   https://raw.github.com/yoshiki/yaml-mode/
(when (require 'yaml-mode nil t)
  (setq auto-mode-alist (cons '("\\.ya?ml$" . yaml-mode) auto-mode-alist)))

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

;; settings for golang
;(require 'go-mode-load)
(add-hook 'go-mode-hook
          '(lambda()
             (setq tab-width 4)
             (setq indent-tabs-mode t)
             (local-set-key (kbd "M-.") 'godef-jump)
             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
             (local-set-key (kbd "C-c i") 'go-goto-imports)
             (local-set-key (kbd "C-c d") 'godoc)))

(add-hook 'before-save-hook 'gofmt-before-save)

;; key bindings
(load "term/bobcat")
(exec-if-bound (terminal-init-bobcat))
;;(global-set-key "\M-j" 'goto-line)
(global-set-key "\M-n" 'browse-url-at-point)
(global-set-key "\M-2" 'make-frame)
(global-set-key "\M-0" 'delete-frame)

;; C-RETで全画面表示(Cocoa Emacs用)
(global-set-key "\C-\M-m" 'ns-toggle-fullscreen)

;;(require 'wdired)
;;(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; migemo
;;(when (require 'migemo nil t)
;;  (when (boundp 'migemo-use-pattern-alist)
;;    (setq migemo-use-pattern-alist t))
;;  (when (boundp 'migemo-use-frequent-pattern-alist)
;;    (setq migemo-use-frequent-pattern-alist t)))

;; ミニバッファ履歴リストの最大長：デフォルトでは30!
(setq history-length t)

;;; session.el
;; kill-ringやミニバッファで過去に開いたファイルなどの履歴を保存する
;(when (require 'session nil t)
;  (setq session-initialize '(de-saveplace session keys menus places))
;  (setq session-globals-include '((kill-ring 50)
;                                  (session-file-alist 500 t)
;                                  (file-name-history 10000)))
;  (add-hook 'after-init-hook 'session-initialize)
;  ;; 前回閉じたときの位置にカーソルを復帰
;  (setq session-undo-check -1))

;; iswitchb
;(iswitchb-mode t)

;; minibuf-isearch: minibufでisearchを使えるようにする。
;;    session.elと組み合わせると便利さを痛感。
;; http://www.sodan.org/~knagano/emacs/minibuf-isearch/
;(require 'minibuf-isearch nil t)

;; mcomplete
;(when (require 'mcomplete nil t)
;  (when (fboundp 'turn-on-mcomplete-mode)
;    (turn-on-mcomplete-mode))
;  (custom-set-faces
;   '(mcomplete-prefix-method-fixed-part-face
;     ((t (:foreground "CadetBlue" :weight bold))))
;   '(mcomplete-prefix-method-alternative-part-face
;     ((t (:foreground "CadetBlue"))))
;   '(mcomplete-substr-method-fixed-part-face
;     ((t (:foreground "MediumVioletRed" :weight bold))))
;   '(mcomplete-substr-method-alternative-part-face
;     ((t (:foreground "MediumVioletRed"))))))

;;(when (require 'completing-help nil t)
;;  (turn-on-completing-help-mode))

;; coloring
(transient-mark-mode t)
(show-paren-mode t)
;; マッチした場合の色
(set-face-background 'show-paren-match-face "RoyalBlue1")
(set-face-foreground 'show-paren-match-face "AntiqueWhite")
;; マッチしていない場合の色
(set-face-background 'show-paren-mismatch-face "Red")
(set-face-foreground 'show-paren-mismatch-face "black")

(put 'narrow-to-region 'disabled nil)

;; color-themeを簡単に使うための設定
(autoload 'color-theme-initialize "color-theme" "Initialize the color theme package by loading color-theme-libraries." t)

;; flymake (Emacs22から標準添付されている)
(when (require 'flymake nil t)
  ;; flymake を使えない場合をチェック
  ;; http://moimoitei.blogspot.jp/2010/05/flymake-in-emacs.html
  (defadvice flymake-can-syntax-check-file
    (after my-flymake-can-syntax-check-file activate)
    (cond
     ((not ad-return-value))
     ;; tramp 経由であれば、無効
     ((and (fboundp 'tramp-list-remote-buffers)
	   (memq (current-buffer) (tramp-list-remote-buffers)))
      (setq ad-return-value nil))
     ;; 書き込み不可ならば、flymakeは無効
     ((not (file-writable-p buffer-file-name))
      (setq ad-return-value nil))
     ;; flymake で使われるコマンドが無ければ無効
     ((let ((cmd (nth 0 (prog1
			    (funcall (flymake-get-init-function buffer-file-name))
			  (funcall (flymake-get-cleanup-function buffer-file-name))))))
	(and cmd (not (executable-find cmd))))
      (setq ad-return-value nil))
     ))

  (global-set-key "\C-cd" 'flymake-display-err-menu-for-current-line)
  (add-hook 'perl-mode-hook
            '(lambda() (flymake-mode t)))
  (add-hook 'cperl-mode-hook
            '(lambda() (flymake-mode t)))
  ;; PHP用設定
  (add-hook 'php-mode-hook
            '(lambda()
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
               ))
  (add-hook 'yaml-mode-hook
            '(lambda()
               ;; YAML用設定
               (when (not (fboundp 'flymake-yaml-init))
                 ;; flymake-yaml-initが未定義のバージョンだったら、自分で定義する
                 (defun flymake-yaml-init ()
                   (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                      'flymake-create-temp-inplace))
                          (local-file (file-relative-name
                                       temp-file
                                       (file-name-directory buffer-file-name))))
                     (list "ruby" (list "-r" "yaml" "-e" "begin;data=YAML.load_file(ARGV[0]);rescue=>e;print e.message;end" local-file))
                     )))
               (setq flymake-allowed-file-name-masks
                     (append
                      flymake-allowed-file-name-masks
                      '(("\\.yaml$" flymake-yaml-init)
                        ("\\.yml$" flymake-yaml-init))))
               (setq flymake-err-line-patterns
                     (cons
                      '("\\(syntax error\\) on line \\([0-9]+\\), col [-0-9]+:" nil 2 nil 1)
                      flymake-err-line-patterns))
               (flymake-mode t)))
  ;; JavaScript用設定
  (add-hook 'javascript-mode-hook
            '(lambda()
               ;; flymake-javascript-initが未定義のバージョンだったら、自分で定義する
               (when (not (fboundp 'flymake-javascript-init))
                 (defun flymake-javascript-init ()
                   (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                      'flymake-create-temp-inplace))
                          (local-file (file-relative-name
                                       temp-file
                                       (file-name-directory buffer-file-name))))
                     ;;(list "js" (list "-s" local-file))
                     (list "jsl" (list "-process" local-file))
                     ))
                 (setq flymake-allowed-file-name-masks
                       (append
                        flymake-allowed-file-name-masks
                        '(("\\.json$" flymake-javascript-init)
                          ("\\.js$" flymake-javascript-init))))
                 (setq flymake-err-line-patterns
                       (cons
                        '("\\(.+\\)(\\([0-9]+\\)): \\(?:lint \\)?\\(\\(?:warning\\|SyntaxError\\):.+\\)" 1 2 nil 3)
                        flymake-err-line-patterns))
                 (flymake-mode t)))))

;; yasnipet
;; *.{c,cxx,css,f,htm,html,pl,php,py,rb} ファイルを初めて読んだときに読み込む
;; http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html#speed
(defadvice find-file (before my-yasnippet-invoke activate)
  (let ((file (ad-get-arg 0)))
    (when (or (string-match "\.c$" file)
              (string-match "\.cxx$" file)
              (string-match "\.css$" file)
              (string-match "\.f$" file)
              (string-match "\.f90$" file)
              (string-match "\.htm$" file)
              (string-match "\.html$" file)
              (string-match "\.pl$" file)
              (string-match "\.php$" file)
              (string-match "\.py$" file)
              (string-match "\.rb$" file))
      (when (require 'yasnippet nil t)
        (exec-if-bound (yas/initialize))
        (exec-if-bound (yas/load-directory (expand-file-name "~/lib/emacs/snippets/"))))
      (ad-deactivate 'find-file)
      (ad-disable-advice 'find-file 'before 'my-yasnippet-invoke) ; 自分を消す
      (ad-activate 'find-file)
      )))

;; yasnippet展開中はflymakeを無効にする
;; http://d.hatena.ne.jp/antipop/20080317/1205766355
(defadvice yas/expand-snippet
  (before inhibit-flymake-syntax-checking-while-expanding-snippet activate)
  (setq flymake-is-active-flag
        (or flymake-is-active-flag
            (assoc-default 'flymake-mode (buffer-local-variables))))
  (when flymake-is-active-flag
    (flymake-mode-off)))
(add-hook 'yas/after-exit-snippet-hook
          '(lambda ()
             (when flymake-is-active-flag
               (flymake-mode-on)
               (setq flymake-is-active-flag nil))))

;; dabbrev/abbrev関連
;; 参考：http://www.math.s.chiba-u.ac.jp/~matsu/emacs/emacs21/abbrev.html
(require 'dabbrev) ; 不要？
(load "dabbrev-ja" t)

;; バックアップファイル（ファイル名~）の設定
(setq backup-enable-predicate
      (lambda (name)
        (cond
         ((eq 0 (string-match (expand-file-name "~/Dropbox/") name)) nil)
         ((eq 0 (string-match (expand-file-name "~/.zsh-functions/") name)) nil)
         ((eq 0 (string-match (file-truename "~/.zsh-functions/") name)) nil)
         ((string-match "^/sudo:[^/]*:/" name) nil)
         ((string-match "^/ssh:[^/]*:/" name) nil)
         ((string-match "^/multi:[^/]*:sudo:[^/]*:/" name) nil)
         (t (normal-backup-enable-predicate name)))))

;; 自動保存ファイル（#ファイル名#）の設定
;; 同一ディレクトリに書き込まれると面倒な場合のための仕組み。
;; sudo以外も、TRAMPのファイル全部同じ扱いにしてもいいかなあ
(setq auto-save-file-name-transforms
      `((".*/Dropbox/.*" ,(expand-file-name "~/tmp/") t)
        ("^/sudo:[^/]*:/.*" ,(expand-file-name "~root/tmp/") t)
        ("^/ssh:[^/]*:/.*" ,(expand-file-name "~/tmp/") t)
        ("^/multi:[^/]*:sudo:.*" ,(expand-file-name "~root/tmp/") t)))

;; auto-save-buffers-enhanced: Emacsでファイルの自動保存
;; http://blog.kentarok.org/entry/20080222/1203688543
;;
;; ただし、TRAMPでssh接続している場合auto-saveと相性が悪すぎるので除外
;; また、auto-saveとhaskell-modeの相性が悪いので*.hsを除外
(when (require 'auto-save-buffers-enhanced nil t)
  (exec-if-bound (auto-save-buffers-enhanced t))
  (setq auto-save-buffers-enhanced-exclude-regexps
        '("^/sshx?:"
          "^/sudo:"
          "\\.hs$")))

;; また、auto-save-buffers-enhancedとhexl-modeの相性が悪いのでオフにする
(add-hook 'hexl-mode-hook
          (lambda ()
            (exec-if-bound (auto-save-buffers-enhanced nil))))

;; keychain ENV setting (for MacOSX 10.4 only?)
;(if (require 'keychain-environment nil t)
;    (eval-after-load "keychain-environment" '(refresh-keychain-environment)))

;; html-modeの、セーブ時にファイルの文字コードをHTMLのMETAタグに合わせて変更する機能をオフにする
(if (boundp 'auto-coding-functions)
    (delete 'sgml-html-meta-auto-coding-function auto-coding-functions))

; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))

;;; MozReplを使って、セーブすると同時にFirefoxをリロード
;;; http://www.emacswiki.org/emacs/MozRepl#toc2
;(when (require 'moz nil t)
;  (defun hook-for-reloading-firefox ()
;    (condition-case ERR
;        (if (and (fboundp 'comint-send-string)
;                 (fboundp 'inferior-moz-process))
;            (comint-send-string
;             (inferior-moz-process)
;             ;; URLのホスト部がlocalhostの場合のみリロード
;             "if (content.location.host == \"localhost\") { BrowserReload(); }"))
;      (error (progn
;               ;; MozReplに接続できなかった場合はremove-hookする。
;               (remove-hook 'after-save-hook 'hook-for-reloading-firefox 'local)
;               (message "removed hook for MozRepl.")))))
;  (defun auto-reload-firefox-on-after-save-hook ()
;    ;; buffer-local
;    (add-hook 'after-save-hook 'hook-for-reloading-firefox 'append 'local))
;  ;;MozReplのポート番号。MozReplの待ち受けポートを変えた場合に適宜変更してください。
;  (setq moz-repl-port 5858)
;  (add-hook 'php-mode-hook 'auto-reload-firefox-on-after-save-hook)
;  (add-hook 'html-mode-hook 'auto-reload-firefox-on-after-save-hook)
;  (add-hook 'css-mode-hook 'auto-reload-firefox-on-after-save-hook))

;(setq tramp-terminal-type "dumb")
;(setq shell-prompt-pattern "^[^>$][>$] *")

;; TRAMPによる自動sudo
;; http://ubulog.blogspot.com/2010/03/emacs-sudo.html
(defun file-root-p (filename)
 "Return t if file FILENAME created by root."
 (eq 0 (nth 2 (file-attributes filename))))

(defun th-rename-tramp-buffer ()
 (when (file-remote-p (buffer-file-name))
   (rename-buffer
    (format "%s:%s"
            (file-remote-p (buffer-file-name) 'method)
            (buffer-name)))))

(add-hook 'find-file-hook
         'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
 "Open FILENAME using tramp's sudo method if it's read-only."
 (if (and (file-root-p (ad-get-arg 0))
          (not (file-writable-p (ad-get-arg 0)))
          (y-or-n-p (concat "File "
                            (ad-get-arg 0)
                            " is read-only.  Open it as root? ")))
     (th-find-file-sudo (ad-get-arg 0))
   ad-do-it))

(defun th-find-file-sudo (file)
 "Opens FILE with root privileges."
 (interactive "F")
 (set-buffer (find-file (concat "/sudo::" file))))

;; settings for illusori's fork of Flymake
;; https://github.com/illusori/emacs-flymake
(require 'flymake)
(setq flymake-max-parallel-syntax-checks 8)
;; flymake-run-in-placeをnilにしてもリモートファイルがうまく扱えず、あきらめた。
;;(setq temporary-file-directory (expand-file-name "~/tmp/emacs/"))
;;(setq flymake-run-in-place nil)

;; If you're a TTY emacs user, flymake-cursor is a must-have.
;; Grab it with apt-get or ports or whatever your OS package manager is.
;; http://www.emacswiki.org/emacs/FlymakeCursor
;; 思ったように動かなかった…
;; (require 'flymake-cursor)

;(setq sql-mysql-program "/usr/local/bin/mysql")
;(setq sql-user "hnw")
;(setq sql-password "")
;(setq sql-server "localhost")
;(setq sql-mysql-options "")

;; anything.el

(defvar org-directory "")
(when (require 'anything nil t)
  (require 'anything-config)
  (require 'anything-match-plugin)
  (require 'anything-complete)
  (exec-if-bound (anything-read-string-mode 1))
  (require 'anything-show-completion)
  (global-set-key "\C-x\C-b" 'anything-filelist+)
  (global-set-key "\M-y" 'anything-show-kill-ring)

  (define-key global-map (kbd "C-x b") 'anything)

  (setq recentf-max-saved-items 500)
  (recentf-mode 1)
)

;; 以前開いたファイルを再度開いたとき、元のカーソル位置を復元する
;; http://www.emacswiki.org/emacs/SavePlace

(when (require 'saveplace nil t)
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places"))

;; gtags

(when (require 'gtags nil t)
  (setq gtags-suggested-key-mapping t)
  (add-hook 'c-mode-common-hook
            '(lambda()
               (gtags-mode 1))))

;; キーバインド
;(setq gtags-mode-hook
;      '(lambda ()
;         (define-key gtags-mode-map "\C-cs" 'gtags-find-symbol)
;         (define-key gtags-mode-map "\C-cr" 'gtags-find-rtag)
;         (define-key gtags-mode-map "\C-ct" 'gtags-find-tag)
;         (define-key gtags-mode-map "\C-cf" 'gtags-parse-file)))

;; open-junk-file

(when (require 'open-junk-file nil t)
  (setq open-junk-file-format "~/Dropbox/junk/%Y/%m/%Y-%m-%d-%H%M%S.")
  (global-set-key (kbd "C-x j") 'open-junk-file))

;; Using customization file
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html

(setq custom-file "~/.emacs.d/conf/customize.el")
(when (file-exists-p (expand-file-name custom-file))
  (load (expand-file-name custom-file) t nil nil))
