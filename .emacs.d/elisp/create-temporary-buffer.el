;;; create-temporary-buffer.el --- Create temporary buffer.
;; URL: http://noqisofon.hatenablog.com/entry/20101102/1288647885

;;; Commentary:

;;; Code:
;; テンポラリバッファを作成し、それをウィンドウに表示します。
(defun create-temporary-buffer ()
  "テンポラリバッファを作成し、それをウィンドウに表示します。"
  (interactive)
  ;; *temp* なバッファを作成し、それをウィンドウに表示します。
  (switch-to-buffer (generate-new-buffer "*temp*"))
  ;; セーブが必要ないことを示します？
  (setq buffer-offer-save nil))

(provide 'create-temporary-buffer)
;;; create-temporary-buffer.el ends here
