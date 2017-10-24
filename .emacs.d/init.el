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
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;; 振る舞いの設定
;; default charset
(prefer-coding-system 'utf-8)

;; saveplace
(require 'saveplace)
(setq-default save-place t)

;; CUA mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; タブ文字を使わない
(setq-default indent-tabs-mode nil)
;; タブ幅の設定
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
          64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

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

;; GUI用設定
(when window-system
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

;; company-mode
(setq company-idle-delay 1.5)
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
