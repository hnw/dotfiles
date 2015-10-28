(global-git-gutter-mode +1)

(custom-set-faces
 '(git-gutter:modified
   ((((class color) (background light))
     (:foreground "DarkOrange" :weight bold))
    (t (:foreground "orange" :weight bold))))
 '(git-gutter:added
   ((((class color) (background light))
     (:foreground "LimeGreen" :weight bold))
    (t (:foreground "green" :weight bold))))
 '(git-gutter:deleted
   ((((class color) (background light))
     (:foreground "VioletRed" :weight bold))
    (t (:foreground "red" :weight bold)))))
