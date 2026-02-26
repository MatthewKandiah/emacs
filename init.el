(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq visible-bell t)

(column-number-mode t)
(delete-selection-mode t)
(global-display-line-numbers-mode t)
(display-battery-mode t)

(set-face-attribute 'default nil :height 140)
(setq scroll-conservatively 1000)

(use-package gruber-darker-theme
  :ensure t
  :init (load-theme 'gruber-darker t))

;; configure backups and autosaves
(setq make-backup-files nil)
(setq auto-save-default nil)

;; please stop trampling all over my config
(setq custom-file "~/.config/emacs/emacs.custom")

;; because I keep accidentally doing this when I want "C-x C-f"
(keymap-global-unset "C-x f")
;; because I keep accidentally doing this when I want "C-x p"
(keymap-global-unset "C-x C-p")

(keymap-global-set "C-c e" 'flymake-show-buffer-diagnostics)
(keymap-global-set "C-c c" 'recompile)
(keymap-global-set "C-c n" 'next-error)
(keymap-global-set "C-c p" 'previous-error)
(keymap-global-set "C-c %" 'mark-page)
(keymap-global-set "M-DEL" 'clear-line)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("elpanongnu" . "https://elpa.nongnu.org/packages")))
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)

(use-package vertico
  :init
  (vertico-mode t))

(use-package which-key
    :defer 0
    :config
    (which-key-mode 1)
    (setq which-key-idle-delay 1))

(use-package magit
  :commands magit-status
  :config (setq magit-save-repository-buffers 'dontask))

(load "~/.config/emacs/odin-mode.el")
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; lsp-mode docs recommend increasing some emacs defaults, they are too low for the lsp client to function properly
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq-default lsp-auto-guess-root t)
  (add-to-list 'lsp-language-id-configuration '(odin-mode . "odin"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "~/.local/bin/ols")
		    :activation-fn (lsp-activate-on "odin")
		    :server-id 'ols
		    :multi-root t))
  :hook
  ((odin-mode . lsp)
   (c-mode . lsp)
   (typescript-ts-mode . lsp)
   (tsx-ts-mode . lsp)
   (python-mode . lsp)
   (kotlin-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(use-package company
  :config
  (global-company-mode t)
  (setq-default company-backends
		'((company-capf company-yasnippet :with company-dabbrev-code company-files)))
  (setq-default company-minimum-prefix-length 0)
  (setq-default company-idle-delay nil)
  (setq-default company-show-quick-access 'left)
  (setq-default company-tooltip-limit 9)
  (setq-default company-require-match nil)
  (setq-default company-frontends '(company-pseudo-tooltip-frontend company-preview-common-frontend))
  (setq-default company-transformers '(company-sort-by-backend-importance))
  :bind(("C-<return>" . 'company-complete)))

(use-package multiple-cursors
  :bind(("C-c C-c" . 'mc/edit-lines)
	("C->" . 'mc/mark-next-like-this)
	("C-<" . 'mc/mark-previous-like-this)
	("C-c C-a" . 'mc/mark-all-like-this)))

(require 'project)

(use-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "ODIN_ROOT")
  (add-to-list 'exec-path-from-shell-variables "LD_LIBRARY_PATH")
  (add-to-list 'exec-path-from-shell-variables "VK_ADD_LAYER_PATH")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(use-package kotlin-mode)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  (yas-global-mode t))

;; let me use these functions please
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun clear-line ()
  (interactive)
  (move-beginning-of-line nil)
  (kill-line))
