;;; package-update-notify --- パッケージの更新を確認し、ユーザーに通知する

;; Copyright (C) 2021  shibafu528

;; Author: shibafu528
;; Package-Requires: ((alert "1.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a cop y of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'package)
(require 'alert)

(defgroup package-update-notify nil "パッケージの更新通知の設定"
  :group 'applications
  :prefix "package-update-notify-")

(defcustom package-update-notify-cache (expand-file-name "package-update-notify" user-emacs-directory)
  "package-update-notifyの実行記録を付けるためのキャッシュファイルの保存先"
  :type 'file)

(defcustom package-update-notify-interval-of-days 7
  "パッケージの更新を自動確認する間隔 (単位: 日)"
  :type 'integer)

(defun package-update-notify-read-cache ()
  "package-update-notifyの実行記録を読み込む"
  (when (file-exists-p package-update-notify-cache)
    (with-temp-buffer
      (insert-file-contents package-update-notify-cache)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun package-update-notify-interval ()
  "パッケージの更新を自動確認する間隔"
  (* 60 60 24 package-update-notify-interval-of-days))

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
  "package-update-notifyの非同期処理完了後のコールバック。"
  (let ((updates (length (package-available-updates))))
    (when (<= 1 updates)
      (alert (format "更新可能なパッケージが %d 個あります" updates)
             :title "package-update-notify")))
  (remove-hook 'package--post-download-archives-hook 'package-update-notify-callback)
  (with-temp-file package-update-notify-cache
    (prin1 (current-time) (current-buffer))))

(defun package-update-notify ()
  "パッケージの更新を確認し、ユーザーに通知する。"
  (interactive)
  (let ((last-checked (package-update-notify-read-cache)))
    (when (or (called-interactively-p 'any)
              (not last-checked)
              (time-less-p (time-add last-checked (package-update-notify-interval)) (current-time)))
      (add-hook 'package--post-download-archives-hook 'package-update-notify-callback)
      (package-refresh-contents t))))

(add-hook 'emacs-startup-hook 'package-update-notify)

(provide 'package-update-notify)
