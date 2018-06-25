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
        ("org" . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; load path
(add-to-list 'load-path
             (expand-file-name "elisp/" user-emacs-directory))

;;; 振る舞いの設定
;; default charset
(prefer-coding-system 'utf-8)

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

;; helm
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

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
  (undohist-initialize))

;; undo-tree
(global-undo-tree-mode)

;; point-undo
(when (require 'point-undo nil t)
  (global-set-key (kbd "M-[") 'point-undo)
  (global-set-key (kbd "M-]") 'point-redo))

;; color-moccur, moccur-edit
(when (require 'color-moccur nil t)
  (setq moccur-split-word t)
  (require 'moccur-edit nil t))

;; wgrep
(require 'wgrep nil t)

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
(global-set-key (kbd "C-x j") 'skk-auto-fill-mode)
(with-eval-after-load 'skk
  (require 'skk-study))

;; ビープ音の消去
(setq ring-bell-function 'ignore)

;;; 見た目周り
;; スプラッシュ非表示
(setq inhibit-startup-message t)

;; uniquify (バッファ名を区別可能にする)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; 括弧ハイライト
(show-paren-mode t)

;; 行ハイライト
;; (CUIでしか好みの表示にならない、要調整)
(unless window-system
  (setq hl-line-face 'underline)
  (global-hl-line-mode t))

;; 行列番号表示
(line-number-mode t)
(column-number-mode t)

;; 改行コードインジケータ
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; 各種UI要素の非表示
(menu-bar-mode 0)
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
  (safe-diminish "helm-mode" 'helm-mode)
  (safe-diminish "smart-cursor-color" 'smart-cursor-color-mode)
  (safe-diminish "undo-tree" 'undo-tree-mode)
  (safe-diminish "which-key" 'which-key-mode))

;; GUI用設定
(when window-system
  ;; frame title
  (setq frame-title-format "%f")
  ;; frame size
  (setq default-frame-alist
        (append (list
                 '(width . 120)
                 '(height . 50))
                default-frame-alist))
  (setq initial-frame-alist default-frame-alist)
  ;; font
  (let ((fonts (font-family-list)))
    (cond ((member "Migu 1M" fonts)
           (set-face-attribute 'default nil :family "Migu 1M" :height 120))
          ((member "VL ゴシック" fonts)
           (set-face-attribute 'default nil :family "VL ゴシック" :height 120))
          ((member "ＭＳ ゴシック" fonts)
           (set-face-attribute 'default nil :family "ＭＳ ゴシック" :height 120))))
  ;; theme
  (load-theme 'misterioso t)
  ;; powerline
  (powerline-default-theme))

;;; コーディング支援
;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; magit
(global-set-key (kbd "C-c g") 'magit-status)

;; git-gutter
;; (linum/nlinumを使う場合、GUIであればfringeなら併用可能だがCUIでは不可?)
(when window-system
  (require 'git-gutter-fringe)
  (global-git-gutter-mode t))

;; company
(setq company-idle-delay 0.5)
(add-hook 'after-init-hook 'global-company-mode)
(defun company-mode-hooks ()
  "Hooks for company-mode."
  ;; define keymap
  (define-key company-mode-map (kbd "<C-tab>") 'company-complete)
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
(setq projectile-completion-system 'helm)
(global-set-key (kbd "C-c p s i") 'projectile-ripgrep)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; php-mode
(setq php-site-url "https://secure.php.net/"
      php-manual-url 'ja)

;; ruby-mode
(defun ruby-mode-hooks ()
  "Hooks for Ruby mode."
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil
        ruby-deep-indent-paren-style nil))
(cond ((executable-find "ruby")
       ;; Rubyがあればenh-ruby-modeに頼る
       (add-hook 'enh-ruby-mode-hook 'ruby-mode-hooks)
       (add-hook 'enh-ruby-mode-hook #'ruby-electric-mode)
       (add-hook 'enh-ruby-mode-hook 'yard-mode)
       (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
       (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))
      (t
       ;; ない環境ではruby-modeの設定を行う
       (add-hook 'ruby-mode-hook 'ruby-mode-hooks)
       (add-hook 'ruby-mode-hook #'ruby-electric-mode)
       (add-hook 'ruby-mode-hook 'yard-mode)))

;; rainbow-delimiters-mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; adoc-mode
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

;;; 環境別の設定
(let ((local-file (expand-file-name "init-local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))
