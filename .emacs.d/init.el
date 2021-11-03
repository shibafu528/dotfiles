;; -*- coding: utf-8 -*-

;; custom-fileの定義
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; init package manager
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; benchmark
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;; load path
(let ((user-lisp-dir (expand-file-name "elisp/" user-emacs-directory)))
  (add-to-list 'load-path user-lisp-dir)
  (let ((default-directory user-lisp-dir))
    (normal-top-level-add-subdirs-to-load-path)))

;; PATHをどうにかする
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; 振る舞いの設定
;; default charset
(prefer-coding-system 'utf-8)

;; transpose系のキーバインドを滅ぼす
(global-unset-key (kbd "C-t")) ; transpose-chars
(global-unset-key (kbd "M-t")) ; transpose-words
(global-unset-key (kbd "C-M-t")) ; transpose-sexps
(global-unset-key (kbd "C-x C-t")) ; transpose-lines

;; macOS向けのModifierリマップ
(when (memq window-system '(mac ns))
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'option))

;; saveplace
(if (>= emacs-major-version 25)
    (save-place-mode 1)
  (require 'saveplace)
  (setq-default save-place t))

;; recentf
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq recentf-max-saved-items 1000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 'never)
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
(recentf-mode t)

;; CUA mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; ivy
(ivy-mode t)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "C-x C-b") 'counsel-recentf)

;; タブ文字を使わない
(setq-default indent-tabs-mode nil)
;; タブ幅の設定
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
          64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; スクロールの設定
(setq scroll-conservatively 1
      scroll-margin 3
      scroll-preserve-screen-position t)
(setq comint-scroll-show-maximum-output t)

;; バックアップとオートセーブの設定
(setq backup-directory (expand-file-name "backups/" user-emacs-directory))
(unless (file-exists-p backup-directory)
  (mkdir backup-directory))
(add-to-list 'backup-directory-alist
             `("." . ,backup-directory))
(setq auto-save-file-name-transforms
      `((".*" ,backup-directory t)))

;; undohist
(when (require 'undohist nil t)
  (setq undohist-ignored-files '("/tmp" "COMMIT_EDITMSG"))
  (undohist-initialize))

;; undo-tree
(global-undo-tree-mode)

;; point-undo
(when (require 'point-undo nil t)
  (global-set-key (kbd "M-[") 'point-undo)
  (global-set-key (kbd "M-]") 'point-redo))

;; which-key
(which-key-setup-side-window-right-bottom)
(which-key-mode t)

;; pcre2el
(add-hook 'prog-mode-hook 'rxt-mode)
(setq reb-re-syntax 'pcre)

;; visual-regexp-steroids
(setq vr/engine 'pcre2el)
(global-set-key (kbd "M-%") 'vr/query-replace)
(global-set-key (kbd "C-M-r") 'vr/isearch-backward)
(global-set-key (kbd "C-M-s") 'vr/isearch-forward)

;; 一時バッファ生成
(autoload 'create-temporary-buffer "create-temporary-buffer" :interactive t)
(global-set-key (kbd "C-c t") 'create-temporary-buffer)

;; open-junk-file
(setq open-junk-file-format "~/junk/%Y%m%d-%H%M%S.")
(global-set-key (kbd "C-c j") 'open-junk-file)

;; SKK
(setq skk-user-directory (expand-file-name "ddskk/" user-emacs-directory))
(setq default-input-method "japanese-skk")
(setq skk-egg-like-newline t)
(global-set-key (kbd "C-x j") 'skk-auto-fill-mode)
(with-eval-after-load 'skk
  (require 'skk-study))

;; yasnippet
(require 'yasnippet)
;; 忘れた、何用だっけ？
;; (when (require 'helm-c-yasnippet nil t)
;;   (setq helm-yas-space-match-any-greedy t)
;;   (global-set-key (kbd "C-c y") 'helm-yas-complete))
(define-key yas-minor-mode-map (kbd "C-c i i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-c i n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-c i v") 'yas-visit-snippet-file)
(yas-global-mode t)

;; ビープ音の消去
(setq ring-bell-function 'ignore)

;; force-revert-buffer
(defun force-revert-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (if (or (not (buffer-modified-p))
          (yes-or-no-p "Revert buffer from file? "))
      (revert-buffer t t)
    (message nil)))
(global-set-key (kbd "C-x RET RET") 'force-revert-buffer)

;;; 見た目周り
;; スプラッシュ非表示
(setq inhibit-startup-message t)

;; uniquify (バッファ名を区別可能にする)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; 括弧ハイライト
(show-paren-mode t)

;; 行ハイライト
(global-hl-line-mode t)
(defun activate-mark-hooks ()
  "Hooks for 'activate-mark-hook'."
  (global-hl-line-mode 0))
(defun deactivate-mark-hooks ()
  "Hooks for 'deactivate-mark-hook'."
  (global-hl-line-mode t)
  ;; なぜかカーソルが黒くなるので再設定する
  (set-cursor-color (foreground-color-at-point)))
(cond (window-system
       ;; TODO: ちょっと明るすぎるかも…
       (set-face-background 'hl-line "#1f5751")
       (set-face-attribute 'hl-line nil :inherit nil)
       ;; リージョンがアクティブな間はハイライトさせない
       (add-hook 'activate-mark-hook 'activate-mark-hooks)
       (add-hook 'deactivate-mark-hook 'deactivate-mark-hooks))
      (t
       (setq hl-line-face 'underline)))

;; インデントハイライト
(setq highlight-indent-guides-method 'column) ; fillとcolumnどっちがいいかなあ
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; 行列番号表示
(line-number-mode t)
(column-number-mode t)

;; 改行コードインジケータ
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; 各種UI要素の非表示
(unless (memq window-system '(mac ns)) ; macOSなら邪魔じゃないから許す
  (menu-bar-mode 0))
(tool-bar-mode 0)

;; 行番号表示
(cond ((>= emacs-major-version 26)
       ;; 26.1以降は標準機能を使用
       (global-display-line-numbers-mode)
       (setq-default display-line-numbers-width 4))
      (t
       ;; 25.3まではnlinumを使用
       (global-nlinum-mode t)
       (setq nlinum-format "%4d ")))

;; ウィンドウ逆順切替
(defun other-window-reverse ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x O") 'other-window-reverse)

;; window-resizer
(autoload 'window-resizer "window-resizer" :interactive t)

;; smart-cursor-color
(smart-cursor-color-mode t)

;; avy
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "M-g h") 'avy-goto-char-timer)

;; diminish
(when (require 'diminish nil t)
  (require 'safe-diminish)
  (safe-diminish "company" 'company-mode)
  (safe-diminish "ivy" 'ivy-mode)
  (safe-diminish "smart-cursor-color" 'smart-cursor-color-mode)
  (safe-diminish "undo-tree" 'undo-tree-mode)
  (safe-diminish "which-key" 'which-key-mode)
  (safe-diminish "yasnippet" 'yas-minor-mode)
  (safe-diminish "editorconfig" 'editorconfig-mode)
  (safe-diminish "eldoc" 'eldoc-mode)
  (safe-diminish "git-gutter" 'git-gutter-mode)
  (safe-diminish "highlight-indent-guides" 'highlight-indent-guides-mode))

;; docker
(global-set-key (kbd "C-c d") 'docker)

;; neotree
(global-set-key (kbd "C-S-n") 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-show-hidden-files t)
(defun neotree-mode-hooks ()
  "Hooks for neotree-mode."
  (display-line-numbers-mode -1))
(add-hook 'neotree-mode-hook 'neotree-mode-hooks)

;; imenu-tree
(autoload 'imenu-tree "imenu-tree" :interactive t)

;; origami
(global-origami-mode)
(define-key origami-mode-map (kbd "M-q") 'origami-recursively-toggle-node)

;; GUI用設定
(when window-system
  ;; frame title
  (setq frame-title-format
        '(:eval (if (buffer-file-name) "%f" "%b")))
  ;; frame size
  (setq default-frame-alist
        (append (list
                 '(width . 120)
                 '(height . 50))
                default-frame-alist))
  (setq initial-frame-alist default-frame-alist)
  ;; font
  (let ((fonts (font-family-list))
        (candidate-fonts '("Migu 1M" "VL ゴシック" "ＭＳ ゴシック"))
        (recognized-font nil))
    (setq recognized-font
          (catch 'recognized
            (dolist (candidate-font candidate-fonts)
              (if (member candidate-font fonts)
                  (throw 'recognized candidate-font)))))
    (when recognized-font
      (set-face-attribute 'default nil :family recognized-font :height 120)
      (set-face-attribute 'fixed-pitch nil :family recognized-font :height 120)
      ;; Macだと真面目にこの辺やらないと中華になっちゃうんだけど、OS問わずやっていいかも?
      (when (memq window-system '(mac ns))
        (let ((fontspec (font-spec :family recognized-font))
              (jp-characters '(katakana-jisx0201
                               cp932-2-byte
                               japanese-jisx0212
                               japanese-jisx0213-2
                               japanese-jisx0213.2004-1)))
          (dolist (jp-character jp-characters)
            (set-fontset-font nil jp-character fontspec))))))
  ;; theme
  (load-theme 'misterioso t)
  ;; powerline
  (powerline-default-theme))

;; org-mode
;; TODO: Dropbox送りにするのもあり
(setq org-todo-keywords '((sequence "TODO" "DOING" "WAITING" "PENDING" "DONE")))
(setq org-default-notes-file (expand-file-name "inbox.org" user-emacs-directory))
(setq org-agenda-files (list org-default-notes-file))
(setq org-adapt-indentation nil)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<hiragana-katakana>") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-<hiragana-katakana>") 'org-capture)
(with-eval-after-load 'org
  (when window-system
    (let ((block-bg "#1F262E"))
      (set-face-background 'org-block-begin-line block-bg)
      (set-face-background 'org-block-end-line block-bg)
      (set-face-background 'org-block block-bg))
    (setq org-src-fontify-natively t)
    (setq org-fontify-quote-and-verse-blocks t))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Inbox")
           "* TODO %?"))))

;; expand-region
(global-set-key (kbd "M-<up>") 'er/expand-region)

;; copy-file-name-to-clipboard
(autoload 'copy-file-name-to-clipboard "copy-file-name-to-clipboard" :interactive t)

;; bm
(global-set-key (kbd "<pause>") 'bm-toggle)
(global-set-key (kbd "C-<prior>") 'bm-previous)
(global-set-key (kbd "C-<next>") 'bm-next)

;; shell-mode
(defun shell-mode-hooks()
  "Hooks for shell-mode."
  (display-line-numbers-mode -1))
(add-hook 'shell-mode-hook 'shell-mode-hooks)

;; dired-mode
(defun dired-mode-hooks()
  "Hooks for dired-mode."
  (display-line-numbers-mode -1))
(add-hook 'dired-mode-hook 'dired-mode-hooks)

;;; コーディング支援
;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(defun flycheck-mode-hooks ()
  "Hooks for flycheck-mode."
  (flycheck-add-mode 'javascript-eslint 'vue-mode)
  (flycheck-add-mode 'javascript-eslint 'vue-html-mode)
  (flycheck-add-mode 'javascript-eslint 'css-mode))
(add-hook 'flycheck-mode-hook #'flycheck-mode-hooks)

;; magit
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "<muhenkan>") 'magit-status)
(setq magit-shortcut-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "l") 'magit-log-buffer-file)
        (define-key map (kbd "b") 'magit-blame)
        map))
(fset 'magit-shortcut-map magit-shortcut-map)
(global-set-key (kbd "C-c m") 'magit-shortcut-map)
(with-eval-after-load "magit-blame"
  (let ((margin (assoc 'margin magit-blame-styles)))
    (delete margin magit-blame-styles)
    (add-to-list 'magit-blame-styles margin)))

;; git-gutter
(cond ((>= emacs-major-version 26)
       ;; 26.1以降は通常のgit-gutter-modeを使用
       (global-git-gutter-mode t)
       (custom-set-variables
        '(git-gutter:modified-sign " ")
        '(git-gutter:added-sign " ")
        '(git-gutter:deleted-sign " "))
       (set-face-background 'git-gutter:modified "purple1")
       (set-face-background 'git-gutter:added "green3")
       (set-face-background 'git-gutter:deleted "red3"))
      ((window-system)
       ;; 25.3まではGUI環境下であればgit-gutter-fringeを使用
       ;; (CUIの場合、fringeが使えずnlinumと干渉するので諦める)
       (require 'git-gutter-fringe)
       (global-git-gutter-mode t)))

;; company
(setq company-idle-delay 0.5)
(add-hook 'after-init-hook 'global-company-mode)
(defun company-mode-hooks ()
  "Hooks for company-mode."
  ;; enable quickhelp
  (company-quickhelp-local-mode t)
  ;; define keymap
  (define-key company-mode-map (kbd "<C-tab>") 'company-complete)
  (dolist (keymap (list company-active-map company-search-map))
    (define-key keymap (kbd "C-n") 'company-select-next)
    (define-key keymap (kbd "C-p") 'company-select-previous))
  ;; set color
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40"))
(add-hook 'company-mode-hook 'company-mode-hooks)

;; projectile
(projectile-mode)
(add-to-list 'projectile-globally-ignored-directories
             "node_modules")
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c p s i") 'projectile-ripgrep)

;; git-complete
(when (require 'git-complete)
  (global-set-key (kbd "C-c C-o") 'git-complete))

;; editorconfig-mode
;; TODO: vue-modeでうまく動作しない。オープン後に(editorconfig-apply)すれば効くけど...
(editorconfig-mode t)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\(\\.php\\)?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

;; emmet-mode
(defun emmet-mode-hooks ()
  "Hooks for Emmet mode."
  (setq emmet-indent-after-insert nil))
(add-hook 'sgml-mode-hook #'emmet-mode)
(add-hook 'web-mode-hook #'emmet-mode)
(add-hook 'emmet-mode-hook #'emmet-mode-hooks)

;; js2-mode
(defun js2-mode-hooks ()
  "Hooks for js2 mode."
  (setq js2-basic-offset 2)
  (setq js-switch-indent-offset 2)
  (setq js2-strict-missing-semi-warning nil))
(add-hook 'js2-mode-hook 'js2-mode-hooks)

;; rjsx-mode
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook #'add-node-modules-path)

;; vue-mode
(add-hook 'vue-mode-hook #'add-node-modules-path)

;; php-mode
(defun php-mode-hooks()
  "Hooks for PHP mode."
  (require 'company-php)
  (ac-php-core-eldoc-setup)
  (set (make-local-variable 'company-backends)
       '((company-ac-php-backend company-dabbrev-code)
         company-capf company-files)))
(add-hook 'php-mode-hook #'php-mode-hooks)
(setq php-site-url "https://secure.php.net/"
      php-manual-url 'ja)

;; ruby-mode
(defun ruby-mode-hooks ()
  "Hooks for Ruby mode."
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil
        ruby-deep-indent-paren-style nil
        ruby-insert-encoding-magic-comment nil)
  ;; rubocopに関する設定
  (make-local-variable 'flycheck-disabled-checkers)
  (if (and (projectile-project-p)
           (file-exists-p (projectile-expand-root ".rubocop.yml")))
      (if (not (file-exists-p (projectile-expand-root ".rubocopfmt-disable")))
          ;; 保存時にauto-correctする
          (rubocopfmt-mode t))
    ;; .rubocop.ymlが設置されていないプロジェクトでは、flycheckでrubocopを使わない
    (add-to-list 'flycheck-disabled-checkers 'ruby-rubocop)))
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'yard-mode)

;; rainbow-delimiters-mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; adoc-mode
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode))

;; rust-mode
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'flycheck-rust-setup)

;; haskell-mode
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)
(defun haskell-interactive-mode-hooks()
  "Hooks for haskell-interactive-mode."
  (display-line-numbers-mode -1))
(add-hook 'haskell-interactive-mode-hook #'haskell-interactive-mode-hooks)

;; mikutter-mode
(autoload 'mikutter-mode "mikutter" :interactive t)
(defun turn-on-mikutter-mode-in-mikutter-dir ()
  (if (and buffer-file-name (string-match "mikutter" buffer-file-name))
      (mikutter-mode)))
(setq mikutter:dir "~/git/mikutter/")
(add-hook 'ruby-mode-hook #'turn-on-mikutter-mode-in-mikutter-dir)

;; Language Server Protocol
;;(load (expand-file-name "init-lsp-mode.el" user-emacs-directory))
(load (expand-file-name "init-eglot.el" user-emacs-directory))

;; それなりに使うコマンドのショートカット
(setq usual-shortcut-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "i") 'projectile-ripgrep)
        (define-key map (kbd "f") 'projectile-find-file)
        (define-key map (kbd "r") 'ripgrep-regexp)
        map))
(fset 'usual-shortcut-map usual-shortcut-map)
(global-set-key (kbd "<henkan>") 'usual-shortcut-map)

;;; 環境別の設定
(let ((local-file (expand-file-name "init-local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))
