;;; copy-file-name-to-clipboard.el

;;; Commentary:

;;; Code:
(defun copy-file-name-to-clipboard ()
  "https://stackoverflow.com/a/2417617"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))
        (x-select-enable-clipboard t))
    (when filename
      (kill-new filename)
      (message filename))))

(provide 'copy-file-name-to-clipboard)
;;; copy-file-name-to-clipboard.el ends here
