;;; safe-diminish.el

;;; Commentary:

;;; Code:
(defmacro safe-diminish (file mode &optional new-name)
  "https://github.com/larstvei/dot-emacs/blob/master/init.org"
  `(with-eval-after-load ,file
     (diminish ,mode ,new-name)))

(provide 'safe-diminish)
;;; safe-diminish.el ends here
