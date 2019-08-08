;;; 20-package.el --- package.el関連

;; http://qiita.com/catatsuy/items/5f1cd86e2522fd3384a0
;; http://www.robario.com/2013/08/07

(defvar my/favorite-packages
  '(
    init-loader
    php-mode
    company-php
    haskell-mode
    csharp-mode
    yaml-mode
    go-mode
    flycheck
    company-go
    go-eldoc
    company
    company-go
    open-junk-file
    gtags
    auto-async-byte-compile
    auto-save-buffers-enhanced
    exec-path-from-shell
    git-gutter
    markdown-mode
    auto-package-update
    swift-mode
    )
  "起動時に自動的にインストールされるパッケージのリスト")

(eval-when-compile
  (require 'cl))

(when (require 'package nil t)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    (when no-ssl
      (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
    ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
      ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
  (package-initialize)
  (let ((pkgs (loop for pkg in my/favorite-packages
                    unless (package-installed-p pkg)
                    collect pkg)))
    (when pkgs
      ;; check for new packages (package versions)
      (message "%s %s" "Get latest versions of all packages..." pkgs)
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (pkg pkgs)
        (package-install pkg))))

  (when (require 'auto-package-update nil t)
    (auto-package-update-maybe)))
