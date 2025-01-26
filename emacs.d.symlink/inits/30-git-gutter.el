(global-git-gutter-mode t)

(custom-set-faces
 '(git-gutter:modified
   ((((class color) (background light))
     (:foreground "purple" :weight bold))
    (t (:foreground "magenta" :weight bold))))
 '(git-gutter:added
   ((((class color) (background light))
     (:foreground "LimeGreen" :weight bold))
    (t (:foreground "green" :weight bold))))
 '(git-gutter:deleted
   ((((class color) (background light))
     (:foreground "VioletRed" :weight bold))
    (t (:foreground "red" :weight bold)))))
