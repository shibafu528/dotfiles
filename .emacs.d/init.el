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
(require 'saveplace)
(setq-default save-place t)

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
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode t)

;; タブ文字を使わない
(setq-default indent-tabs-mode nil)
;; タブ幅の設定
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
          64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

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
(require 'create-temporary-buffer)
(global-set-key (kbd "C-c t") 'create-temporary-buffer)

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

;; nlinum
(global-nlinum-mode t)
(setq nlinum-format "%4d ")

;; window-resizer
(require 'window-resizer)

;; smart-cursor-color
(smart-cursor-color-mode t)

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
  (cond ((member "Migu 1M" (font-family-list))
         (set-face-attribute 'default nil :family "Migu 1M" :height 120))
        ((member "VL ゴシック" (font-family-list))
         (set-face-attribute 'default nil :family "VL ゴシック" :height 120))
        ((member "ＭＳ ゴシック" (font-family-list))
         (set-face-attribute 'default nil :family "ＭＳ ゴシック" :height 120)))
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
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)
(add-hook 'ruby-mode-hook #'ruby-electric-mode)

;; rainbow-delimiters-mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; 環境別の設定
(let ((local-file (expand-file-name "init-local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))

