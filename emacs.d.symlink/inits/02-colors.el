;; カーソルの色の設定。普段は"dim gray"、日本語のときは"brown"
(setq my-roman-cursor-color "dim gray")
(setq my-japanese-cursor-color "brown")

;; color-themeを簡単に使うための設定
(autoload 'color-theme-initialize "color-theme" "Initialize the color theme package by loading color-theme-libraries." t)

;; paren-mode
;; マッチした場合の色
(set-face-background 'show-paren-match-face "RoyalBlue1")
(set-face-foreground 'show-paren-match-face "AntiqueWhite")
;; マッチしていない場合の色
(set-face-background 'show-paren-mismatch-face "Red")
(set-face-foreground 'show-paren-mismatch-face "black")
