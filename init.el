(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

(setq scroll-conservatively 1000)

(use-package gruber-darker-theme
  :ensure t
  :init (load-theme 'gruber-darker t))

;; configure backups and autosaves
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/autosaves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; please stop trampling all over my config
(setq custom-file "~/.config/emacs/emacs.custom")

;; because I keep accidentally doing this when I want "C-x C-f"
(keymap-global-unset "C-x f")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("elpanongnu" . "https://elpa.nongnu.org/packages")))
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

(load "~/.config/emacs/odin-mode.el")
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq-default lsp-auto-guess-root t)
  (add-to-list 'lsp-language-id-configuration '(odin-mode . "odin"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "~/.local/bin/ols")
		    :activation-fn (lsp-activate-on "odin")
		    :server-id 'ols
		    :multi-root t))
  :hook
  ((odin-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay nil)
  :bind(("C-<tab>" . 'company-complete)))
  

;; TODO-Matt
;; - term / vterm / eshell for running commands and using the terminal output easily
;; - install multiple-cursor see if it's worth using
;; - keymaps for fuzzy finding, and opening dired at the current file, and using multiple cursors
;; - wtf does projectile actually do?
