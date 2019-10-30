;; -*- lexical-binding: t -*-
;; open-junk-file

(defvar my/junk-file-dirs
  '(
    "/Volumes/GoogleDrive/マイドライブ/Sync"
    "~/OneDrive"
    "~/Dropbox"
    "~/"
    ))
(when (require 'open-junk-file nil t)
  (let ((dirs my/junk-file-dirs)
        (first-dir nil)
        (first-format nil)
        (second-dir nil)
        (second-format nil))
    (setq dirs (mapcan (lambda (x) (and (file-directory-p x) (list x))) dirs))
    (setq first-dir (car dirs))
    (when first-dir
      ;; 最初に見つかったディレクトリにC-x jでファイル作成
      (setq first-format (concat first-dir "/junk/%Y/%m/%Y-%m-%d-%H%M%S."))
      (setq open-junk-file-format first-format)
      (global-set-key (kbd "C-x j") 'open-junk-file)
      (setq second-dir (car (cdr dirs)))
      (when second-dir
        ;; 2番目に見つかったディレクトリにC-x C-jでファイル作成
        (setq second-format (concat second-dir "/junk/%Y/%m/%Y-%m-%d-%H%M%S."))
        (global-set-key (kbd "C-x C-j")
                        (lambda () (interactive)
                          (open-junk-file second-format)))))))
