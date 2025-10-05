(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(load-theme 'leuven-dark)
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

(setq scroll-conservatively 1000)

;; check these are actually used
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

(require 'ido)
(setq ido-everywhere t)
(ido-mode t)

(use-package which-key
    :defer 0
    :config
    (which-key-mode 1)
    (setq which-key-idle-delay 1))

(use-package magit
  :commands magit-status)

;;(use-package multiple-cursors)

;; TODO-Matt
;; - investigate window splitting and moving between them / rearranging them (maybe add keymaps?)
;; - investigate actually using dired
;; - investigate actually using magit
;; - investigate compilation mode
;; - term / vterm / eshell for running commands and using the terminal output easily
;; - keymaps for fuzzy finding, and opening dired at the current file, and using multiple cursors
;; - wtf does projectile actually do?
;; - wtf does ivy do?
;; - wtf does company actually do?

;; is this the right way to do this?
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
