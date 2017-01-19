;;; フォントサイズ・フレームサイズの設定
(when window-system
  ;; フォントサイズ・フレームサイズを定義する連想配列
  ;; (解像度 フォントサイズ フレーム幅 フレーム高)で定義
  ;; 解像度について、マルチディスプレイ環境ではEmacsの実装ごとに取れる値が変わる模様。
  ;; 適宜なんとかしてください。
  (setq my-resolution-alist
        '(
          ("1440x900@25" 16  nil 37)   ; 16pt,81列37行(実質32行), MB12@CocoaEmacs
          ("1440x900@24" 16  nil 37)   ; 16pt,81列37行(実質32行), MBA13@CocoaEmacs
          ;("1440x900@24" 24  nil 32)   ; 24pt,81列32行(実質27行), MBA13@CocoaEmacs
          ("1366x768@24" 16  nil 37)   ; 16pt,81列37行(実質34行), MBA11@CocoaEmacs
          ("1366x768@23" 16  nil 39)   ; 16pt,81列39行(実質34行), MBA11@CocoaEmacs
          ;("1366x768@23" 24  nil 32)   ; 24pt,81列32行, MBA11@CocoaEmacs, プレゼン用
          ("1366x768@22" 16  nil 34)   ; 16pt,81列34行, MBA11@CarbonEmacs
          ("1280x800@22" nil nil 45)   ; 14pt,81列45行, MBP@CarbonEmacs
          (nil           14  81  59))) ; 14pt,81列59行, デフォルト設定

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
