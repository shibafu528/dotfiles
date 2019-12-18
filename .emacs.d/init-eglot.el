;;; ------------------------------------
;;; eglotに関する設定
;;; ------------------------------------

(defun eglot-ensure-and-turn-off-flycheck ()
  (eglot-ensure)
  (flycheck-mode -1))

;; gopls -> $ go get -u golang.org/x/tools/cmd/gopls
(add-hook 'go-mode-hook 'eglot-ensure-and-turn-off-flycheck)

;; solargraph -> $ gem install solargraph
(add-hook 'ruby-mode-hook 'eglot-ensure) ; rubocopがflycheckじゃないと動かないんだけど、なんで?
;; (with-eval-after-load 'eglot
;;   (defclass eglot-solargraph (eglot-lsp-server) ())
;;   (cl-defmethod eglot-initialization-options ((server eglot-solargraph))
;;     '(:solargraph (:diagnostics t :useBundler :json-false)))
;;   (add-to-list 'eglot-server-programs '(ruby-mode . (eglot-solargraph "solargraph" "socket" "--port" :autoport))))

;; typescript-language-server -> $ yarn global add typescript typescript-language-server
(add-hook 'js2-mode-hook 'eglot-ensure-and-turn-off-flycheck)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(js2-mode . ("typescript-language-server" "--stdio"))))
