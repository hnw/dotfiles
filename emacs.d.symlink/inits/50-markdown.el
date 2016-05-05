(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(setq markdown-command-needs-filename t)
(setq markdown-open-command "code")
