;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manual configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 user-full-name "Emmanuel Oga"
 user-mail-address "EmmanuelOga@gmail.com")

;; Shift-Arrows to move windows.
(windmove-default-keybindings)

;; Tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Parens
(show-paren-mode)

;; Always use unix line endings.
(defun always-unix-newlines ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))
(add-hook 'find-file-hooks 'always-unix-newlines)

;; Disable frequency of GC. This helps performance both during init
;; and after init. Value is in bytes so this is 100MB, as suggested in
;; <https://github.com/emacs-lsp/lsp-mode#performance>.
(setq gc-cons-threshold (* 100 1024 1024))

;; Always load newest byte code
(setq load-prefer-newer t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(toggle-scroll-bar -1) 
(tool-bar-mode -1) 
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Dim unused buffers
(use-package dimmer
  :straight t
  :init
  (dimmer-mode t))

(use-package rainbow-delimiters :straight t)

;; File and outline tree.
(use-package treemacs
  :straight t
  :config
  (global-set-key [f8] 'treemacs))

(use-package treemacs-evil :straight t)

(use-package evil
  :straight t

  :init
  ;; Evil: use C-z to enter an exit evil mode.
  (global-unset-key [(control z)])
  (global-unset-key [(control x)(control z)])

  :config
  (setq evil-default-state 'emacs)
  (evil-mode 1)
  ;; Remove enter and space from the motion map.
  (defun -move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location. "
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))
  (-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  (-move-key evil-motion-state-map evil-normal-state-map " "))

(use-package no-littering :straight t)
(use-package zerodark-theme :straight t)

(use-package selectrum
  :straight t
  :defer t
  :init
  ;; This doesn't actually load Selectrum.
  (selectrum-mode +1))

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :straight t
  :config
  (prescient-persist-mode +1)
  (setq prescient-history-length 1000))

;; Package `selectrum-prescient' provides intelligent sorting and
;; filtering for candidates in Selectrum menus.
(use-package selectrum-prescient
  :straight t
  :demand t
  :after selectrum
  :config
  (selectrum-prescient-mode +1))

;; F3 to highlight symbol under point.
(use-package highlight-symbol
  :straight t
  :config
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

;; Highlight cursor on move.
(use-package beacon
  :straight t
  :init
  (beacon-mode))

(use-package cider
  :straight t
  :config
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'rainbow-delimiters-mode)

  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)

  (setq cider-jdk-src-paths '("C:/java/jdk-13.0.2+8/source"
                              "C:/java/jdk-13.0.2+8/source/java.base"
                              "C:/java/saxon/9-9-1-6-source")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables changed through customization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes '(zerodark))
 '(custom-safe-themes
   '("846ef3695c42d50347884515f98cc359a7a61b82a8d0c168df0f688cf54bf089" "48c8e318a70466bb6d46509c8d6d458c047ee8107541b8de973cb3cb048b9592" default))
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
 '(default ((t (:family "Source Code Pro" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#f00")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
