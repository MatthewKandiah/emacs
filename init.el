(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)
(load-theme 'leuven-dark)
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; think I broke these...
;;(auto-save-file-name-transforms '((".*" "~/.config/emacs/autosaves/" t)))
;;(backup-directory-alist '((".*" . "~/.config/emacs/backups/")))
(make-directory "~/.config/emacs/autosaves/" t)
(make-directory "~/.config/emacs/backups/" t)

(setq custom-file "~/.config/emacs/emacs.custom")

;; because I keep accidentally doing this when I want "C-x C-f"
(keymap-global-unset "C-x f")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)

;; Disable line numbers in some modes
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package which-key
    :defer 0
    :config
    (which-key-mode 1)
    (setq which-key-idle-delay 1))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package ivy
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ivy
  :after lsp)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; not actually sure what this does for us
(use-package projectile
  :config (projectile-mode 1)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  ;; skipped bit about search path - possibly worth revisiting!
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :commands magit-status)

;; TODO-Matt
;; - investigate actually using dired
;; - investigate actually using magit
;; - investigate compilation mode
;; - term / vterm / eshell for running commands and using the terminal output easily
;; - keymaps for fuzzy finding, and opening dired at the current file

(load "~/.config/emacs/odin-mode.el")

(with-eval-after-load 'lsp-mode
  (setq-default lsp-auto-guess-root t)
  (setq lsp-language-id-configuration (cons '(odin-mode . "odin") lsp-language-id-configuration))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "~/.local/bin/ols")
		    :major-modes '(odin-mode)
		    :server-id 'ols
		    :multi-root t)))
(add-hook 'odin-mode-hook #'lsp)
