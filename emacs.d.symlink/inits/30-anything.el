;; anything.el

(defvar org-directory "")
(when (require 'anything nil t)
  (require 'anything-config)
  (require 'anything-match-plugin)
  (require 'anything-complete)
  (anything-read-string-mode 1)
  (require 'anything-show-completion)
  (global-set-key "\C-x\C-b" 'anything-filelist+)
  (global-set-key "\M-y" 'anything-show-kill-ring)

  (define-key global-map (kbd "C-x b") 'anything)

  (setq recentf-max-saved-items 500)
  (recentf-mode 1)
  )

(when (require 'anything-complete nil t)
  (setq anything-complete-sort-candidates t)
  (substitute-key-definition 'execute-extended-command
                             'anything-execute-extended-command global-map))
