;;; ------------------------------------
;;; lsp-modeに関する設定
;;; ------------------------------------

(setq lsp-keymap-prefix "C-c l")

(defun lsp-deferred-and-turn-off-flycheck ()
  (lsp-deferred)
  (flycheck-mode -1))

;; lsp-mode
(defun lsp-mode-hooks ()
  "Hooks for lsp-mode."
  (setq lsp-auto-guess-root t)
  (lsp-treemacs-sync-mode t))
(add-hook 'lsp-mode-hook 'lsp-mode-hooks)

;; rls -> $ rustup component add rls rust-analysis rust-src
;;(add-hook 'rust-mode-hook 'lsp-deferred)

;; gopls -> $ go get -u golang.org/x/tools/cmd/gopls
(add-hook 'go-mode-hook 'lsp-deferred)

;; solargraph -> $ gem install solargraph
(add-hook 'ruby-mode-hook 'lsp-deferred-and-turn-off-flycheck)

;; typescript-language-server -> $ yarn global add typescript typescript-language-server
(add-hook 'js-mode-hook 'lsp-deferred)

;; lsp-ui
(defun toggle-lsp-ui-doc ()
  (interactive)
  (if lsp-ui-doc-mode
      (progn
        (lsp-ui-doc-mode -1)
        (lsp-ui-doc--hide-frame))
    (lsp-ui-doc-mode t)))
(defun lsp-ui-mode-hooks ()
  "Hooks for lsp-ui-mode."
  ;; lsp-ui-doc
  (setq lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t)
  ;; lsp-ui-sideline
  (setq lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-symbol nil)
  ;; keymap
  (define-key lsp-ui-mode-map (kbd "C-c l d") 'toggle-lsp-ui-doc)
  (define-key lsp-ui-mode-map (kbd "C-c l s") 'lsp-ui-sideline-mode)
  (define-key lsp-ui-mode-map (kbd "C-c l i") 'lsp-ui-imenu))
(add-hook 'lsp-ui-mode-hook 'lsp-ui-mode-hooks)
