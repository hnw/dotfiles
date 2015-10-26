;;; シェルの環境変数を取り込む
;;; http://d.hatena.ne.jp/syohex/20130718/1374154709
(when (require 'exec-path-from-shell nil t)
  (let ((envs '("PATH" "GOPATH")))
    (exec-path-from-shell-copy-envs envs)))
