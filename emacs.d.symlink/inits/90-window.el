;;; フォントサイズ・フレームサイズの設定
(when window-system
  ;; フォントサイズ・フレームサイズを定義する連想配列
  ;; (解像度 フォントサイズ フレーム幅 フレーム高)で定義
  ;; 解像度について、マルチディスプレイ環境ではEmacsの実装ごとに取れる値が変わる模様。
  ;; 適宜なんとかしてください。
  (setq my-resolution-alist
        '(
          ("4880x1440@29" 16 nil 40)   ; 16pt,81列40行(実質34行), MBP+display
          ("1440x900@29"  14 nil 43)   ; 14pt,81列43行(実質38行), MBP
          ("3008x1692@29" 20 nil 60)   ; 20pt,81列60行(実質51行), MacMini
          (nil            14 nil 59))) ; 14pt,81列59行, デフォルト設定

  ;; 解像度+Emacsバージョン, 例:1366x768@23
  (setq main-display-resolution
        (format "%dx%d@%d"
                (x-display-pixel-width)
                (x-display-pixel-height)
                emacs-major-version))
  (defvar my-font-size nil)
  (let* ((list-merge
          (lambda (list1 list2)
            (if (and (not list1) (not list2))
                nil
              (cons (or (car list1) (car list2))
                    (funcall list-merge (cdr list1) (cdr list2))))))
         (resolution-list (funcall list-merge
                                   (cdr (assoc main-display-resolution my-resolution-alist))
                                   (cdr (assoc nil my-resolution-alist))))
         (frame-width-alist (if (nth 1 resolution-list)
                                (list (cons 'width (nth 1 resolution-list)))
                              nil))
         (frame-height-alist (if (nth 2 resolution-list)
                                 (list (cons 'height (nth 2 resolution-list)))
                               nil))
         (frame-size-alist (append frame-width-alist
                                   frame-height-alist)))
    ;; フォントサイズの設定
    (setq my-font-size (car resolution-list))
    ;; 初期フレームの位置設定
    (setq initial-frame-alist
          (append
           '((top  . 26)    ; フレームの Y 位置(ピクセル数)
             (left . 30))   ; フレームの X 位置(ピクセル数)
           initial-frame-alist))
    ;; フレームのサイズ設定
    (setq default-frame-alist
          (append
           frame-size-alist
           default-frame-alist))
))
