(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq visible-bell t)

(column-number-mode t)
(delete-selection-mode t)
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

;; think I'll use this as my "leader key" - default bound to `transpose-chars`
(keymap-global-unset "C-t")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("elpanongnu" . "https://elpa.nongnu.org/packages")))
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)

(use-package which-key
    :defer 0
    :config
    (which-key-mode 1)
    (setq which-key-idle-delay 1))

(use-package ivy
  :config
  (ivy-mode 1))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode))

(use-package lsp-ivy
  :after lsp)

(use-package magit
  :commands magit-status)

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
   (python-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 0)
  (company-idle-delay nil)
  :bind(("C-<return>" . 'company-complete)))

(require 'recentf)
(global-set-key (kbd "C-x C-r") 'recentf-open)
(recentf-mode t)
(setq recentf-max-saved-items 100)

(use-package multiple-cursors
  :bind(("C-c C-c" . 'mc/edit-lines)
	("C->" . 'mc/mark-next-like-this)
	("C-<" . 'mc/mark-previous-like-this)
	("C-c C-a" . 'mc/mark-all-like-this)))

(require 'project)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package speed-type)

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; TODO-MATT: add to C-t keymap for easy use
(defun insert-todo ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (insert "\n")
    (previous-line)
    (insert "TODO-MATT")
    (comment-line 1)))
