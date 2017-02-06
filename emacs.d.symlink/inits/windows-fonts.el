;; via: http://ntemacsjp.osdn.jp/matsuan/FontSettingJp.html

(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 120)

(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("ＭＳ ゴシック*" . "jisx0208-sjis"))

(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("ＭＳ ゴシック*" . "jisx0201-katakana"))

(setq w32-enable-synthesized-fonts t)

(add-to-list 'face-font-rescale-alist
             `(,(encode-coding-string ".*ＭＳ.*bold.*iso8859.*" 'emacs-mule) . 0.9))

(add-to-list 'face-font-rescale-alist
             `(,(encode-coding-string ".*ＭＳ.*bold.*jisx02.*" 'emacs-mule) . 0.95))
