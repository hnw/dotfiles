;;; 20-package.el --- package.el関連

;; http://qiita.com/catatsuy/items/5f1cd86e2522fd3384a0
;; http://www.robario.com/2013/08/07

(defvar my/favorite-packages
  '(
    init-loader
    php-mode
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
    anything
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
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize)
  (let ((pkgs (loop for pkg in my/favorite-packages
                    unless (package-installed-p pkg)
                    collect pkg)))
    (when pkgs
      ;; check for new packages (package versions)
      (message "%s" "Get latest versions of all packages...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (pkg pkgs)
        (package-install pkg))))

  (when (require 'auto-package-update nil t)
    (auto-package-update-maybe)))
