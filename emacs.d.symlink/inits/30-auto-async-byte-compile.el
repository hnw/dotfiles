(when (require 'auto-async-byte-compile nil t)
  (setq auto-async-byte-compile-init-file
        (concat user-emacs-directory "async-compile-init.el"))
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))
