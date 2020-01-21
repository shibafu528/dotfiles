;; my scratchpad

(defun remote-url-to-github-url (url)
  (cond ((string-match "^git@github\\.com:\\([a-zA-Z_.-]+\\/[a-zA-Z_.-]+\\)\\.git$" url)
         (concat "https://github.com/" (match-string 1 url)))
        ((string-match "^https:\\/\\/github\\.com\\/\\([a-zA-Z_.-]+\\/[a-zA-Z_.-]+\\)\\.git$" url)
         (concat "https://github.com/" (match-string 1 url)))))
