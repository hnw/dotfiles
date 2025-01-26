;; http://www.sodan.org/~knagano/emacs/dotemacs.html
;; （バイトコンパイル時のwarningを抑止する目的に使っている）

(defmacro exec-if-bound (sexplist)
  "関数が存在する時だけ実行する（car の fboundp を調べるだけ）"
  `(if (fboundp (car ',sexplist))
       ,sexplist))

(eval-when-compile
  (require 'cl))

; init.elcよりinit.elの方が新しかったら再度byte-compileする
; http://emacswiki.org/emacs/AutoRecompile
(defun byte-compile-user-init-file-if-needed ()
  (interactive)
  (cond
   ((equal (file-name-extension user-init-file) "elc")
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
        (delete-other-windows))))
   (t
    (byte-compile-file user-init-file))))

;(byte-compile-user-init-file-if-needed)


;;;フォント設定

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

; (c-basic-offset . 2) はしばらく封印

;; settings for YAML
;;   yaml-mode は下記を利用。
;;   https://raw.github.com/yoshiki/yaml-mode/
(when (require 'yaml-mode nil t)
  (setq auto-mode-alist (cons '("\\.ya?ml$" . yaml-mode) auto-mode-alist)))

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

(put 'narrow-to-region 'disabled nil)


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

;; ロックファイル（.#foo.txt）の設定
;; 参考：https://apribase.net/2024/07/22/emacs-auto-save/
(setq create-lockfiles nil) ; 作らない

;; 自動セーブファイル（#foo.txt#）の設定
;; 参考：https://apribase.net/2024/07/22/emacs-auto-save/
(setup files
  (:opt auto-save-default nil)) ; 作らない

;; バックアップファイル（ファイル名~）の設定
(setq backup-enable-predicate
      (lambda (name)
        (cond
         ((eq 0 (string-match "/Volumes/GoogleDrive/マイドライブ/Sync/" name)) nil)
         ((eq 0 (string-match ((file-truename (expand-file-name "~/OneDrive/")) name)) nil)
         ((eq 0 (string-match (expand-file-name "~/Dropbox/") name)) nil)
         ((eq 0 (string-match (expand-file-name "~/.zsh-functions/") name)) nil)
         ((eq 0 (string-match (file-truename "~/.zsh-functions/") name)) nil)
         ((string-match "^/sudo:[^/]*:/" name) nil)
         ((string-match "^/ssh:[^/]*:/" name) nil)
         ((string-match "^/multi:[^/]*:sudo:[^/]*:/" name) nil)
         (t (normal-backup-enable-predicate name)))))

;; keychain ENV setting (for MacOSX 10.4 only?)
;(if (require 'keychain-environment nil t)
;    (eval-after-load "keychain-environment" '(refresh-keychain-environment)))

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

;; Using customization file
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html

(setq custom-file (concat user-emacs-directory "conf/customize.el"))
(when (file-exists-p (expand-file-name custom-file))
  (load (expand-file-name custom-file) t nil nil))
