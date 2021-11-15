;;; package-update-notify --- パッケージの更新を確認し、ユーザーに通知する

(require 'package)
(require 'json)

(defvar package-update-notify-cache (expand-file-name "package-update-notify" user-emacs-directory)
  "package-update-notifyの実行記録を付けるためのキャッシュファイルの保存先")

(defun package-available-updates ()
  "更新のあるパッケージのリストを作成する。"
  (let (updates
        (archives (make-hash-table)))
    ;; リポジトリのパッケージ情報をHash化
    (dolist (pkg package-archive-contents)
      (puthash (car pkg) (cadr pkg) archives))
    ;; インストール済のパッケージと比較
    (dolist (pkg package-alist)
      (pcase pkg
        (`(,name ,installed)
         (let ((upstream (gethash name archives)))
           (when (and upstream
                      (version-list-< (package-desc-version installed) (package-desc-version upstream)))
             ;; アップデートがある
             (add-to-list 'updates (cons name upstream) t))))))
    updates))

(defun package-update-notify-callback ()
  "package-update-notify の非同期処理完了後のコールバック。"
  (let ((updates (length (package-available-updates))))
    (when (<= 1 updates)
      (if (executable-find "notify-send")
          (start-process "package-update-notify"
                         nil
                         "notify-send"
                         "-a" "Emacs"
                         "-t" "10000"
                         "-h" "string:desktop-entry:emacs"
                         (format "更新可能なパッケージが %d 個あります" updates))
        (message "更新可能なパッケージが %d 個あります" updates))))
  (remove-hook 'package--post-download-archives-hook 'package-update-notify-callback)
  (with-temp-file package-update-notify-cache
    (insert (json-encode (current-time)))))

(defun package-update-notify ()
  "パッケージの更新を確認し、ユーザーに通知する。"
  (interactive)
  ;; TODO: ファイルないときの処理
  (let ((last-checked (append (json-read-file package-update-notify-cache) nil)))
    (when (or (time-less-p (time-add last-checked (* 60 60 24 7)) (current-time))
              (called-interactively-p 'any))
      (add-hook 'package--post-download-archives-hook 'package-update-notify-callback)
      (package-refresh-contents t))))

;; TODO: 毎回起動時に実行するのはアレなので、時々実行するような仕組みを作ってからフックかける
;; (add-hook 'emacs-startup-hook 'package-update-notify)

(provide 'package-update-notify)
