;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manual configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 user-full-name "Emmanuel Oga"
 user-mail-address "EmmanuelOga@gmail.com")

;; Shift-Arrows to move windows.
(windmove-default-keybindings)

;; `winner' provides an undo/redo stack for window configurations,
;; with undo and redo being C-c left and C-c right,
;; respectively.
(winner-mode +1)

;; Remember last cursor position on file.
(save-place-mode +1)

;; When visiting a file, revert it if it has updated and no edits and
;; it has changed.
(global-auto-revert-mode +1)

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

;; Don't make lockfiles: those annoying files with # in the path!
(setq create-lockfiles nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package `buffer-move' provides simple commands to swap Emacs
;; windows: `buf-move-up', `buf-move-down', `buf-move-left',
;; `buf-move-right'.
(use-package buffer-move :straight t)

;; Dim unused buffers
(use-package dimmer
  :straight t
  :init
  (dimmer-mode t))

(use-package rainbow-delimiters :straight t)

(use-package magit :straight t)

;; File and outline tree.
(use-package treemacs
  :straight t
  :config
  (global-set-key [f8] 'treemacs)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple))))

(use-package treemacs-evil        :straight t :after treemacs evil)
(use-package treemacs-magit       :straight t :after treemacs magit)
(use-package treemacs-projectile  :straight t :after treemacs projectile)
(use-package treemacs-icons-dired :straight t :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package evil
  :straight t

  :init
  ;; Evil: use C-z to enter an exit evil mode.
  (global-unset-key [(control z)])
  (global-unset-key [(control x)(control z)])

  :config
  (setq evil-default-state 'emacs)
  (evil-mode 1)

  (evil-set-initial-state 'xquery-mode 'normal)
  (evil-set-initial-state 'markdown-mode 'normal)
  (evil-set-initial-state 'java-mode 'normal)
  (evil-set-initial-state 'clojure-mode 'normal)
  (evil-set-initial-state 'emacs-lisp-mode 'normal)
  (evil-set-initial-state 'nxml-mode 'normal)
  (evil-set-initial-state 'ttl-mode 'normal)
  (push '("magit*" . emacs) evil-buffer-regexps)
  
  ;; Remove enter and space from the motion map.
  (defun -move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location. "
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))
  (-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  (-move-key evil-motion-state-map evil-normal-state-map " "))

(use-package no-littering :straight t)

(use-package xquery-mode :straight t)

(use-package zerodark-theme :straight t)

(use-package selectrum
  :straight t
  :defer t
  :init
  ;; This doesn't actually load Selectrum.
  (selectrum-mode +1))

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  :init
  (global-company-mode +1))

(use-package company-box
  :straight t
  :after company
  :hook
  (company-mode . company-box-mode))

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :straight t
  :after company-box
  :config
  (prescient-persist-mode +1)
  (setq prescient-history-length 1000))

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :straight t
  :after company-box
  :config
  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1))

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

(use-package clojure-mode :straight t)

;; Package `helpful' provides a complete replacement for the built-in
;; Emacs help facility which provides much more contextual information
;; in a better format.
(use-package helpful
  :straight t
  :bind
  (;; Remap standard commands.
   ([remap describe-function] . #'helpful-callable)
   ([remap describe-variable] . #'helpful-variable)
   ([remap describe-symbol]   . #'helpful-symbol)
   ([remap describe-key]      . #'helpful-key)

   ;; Suggested bindings from the documentation at
   ;; https://github.com/Wilfred/helpful.

   :map help-map
   ("F" . #'helpful-function)
   ("M-f" . #'helpful-macro)
   ("C" . #'helpful-command)

   :map global-map
   ("C-c C-d" . #'helpful-at-point)))

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

(use-package hydra
  :straight t)

(use-package cider-hydra
  :straight t
  :after hydra
  :config
  (add-hook 'clojure-mode-hook #'cider-hydra-mode))

(use-package yasnippet
  :straight t)

(use-package clj-refactor
  :straight t
  :after yasnippet
  :config
  (defun clj-refactor-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'clj-refactor-hook))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package flycheck :straight t)

(use-package which-key
  :straight t
  :config (which-key-mode))

(use-package smooth-scrolling
  :straight t
  :config
  (smooth-scrolling-mode 1))

(use-package gist :straight t)

(use-package deadgrep :straight t)

;; Various language modes.
(use-package dockerfile-mode :straight t)
(use-package gitconfig-mode :straight t)
(use-package gitignore-mode :straight t)
(use-package go-mode :straight t)
(use-package json-mode :straight t)
(use-package pip-requirements :straight t)
(use-package pkgbuild-mode :straight t)
(use-package ssh-config-mode :straight t)
(use-package terraform-mode :straight t)
(use-package toml-mode :straight t)
(use-package yaml-mode :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP Mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-l")

(use-package lsp-mode
  :straight t
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :straight t)

(use-package ttl-mode
  :straight (:host github :repo "jeeger/ttl-mode")
  :config
  (autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)

  ; Turn on font lock when in ttl mode
  (add-hook 'ttl-mode-hook 'turn-on-font-lock)

  (setq auto-mode-alist
        (append
         (list
          '("\\.n3" . ttl-mode)
          '("\\.ttl" . ttl-mode))
         auto-mode-alist)))

;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; ...

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
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "846ef3695c42d50347884515f98cc359a7a61b82a8d0c168df0f688cf54bf089" "48c8e318a70466bb6d46509c8d6d458c047ee8107541b8de973cb3cb048b9592" default))
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
 '(show-paren-match ((t (:background "ivory" :foreground "firebrick" :weight ultra-bold)))))

