;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; EMACS CONFIGURATION ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Loading .emacs")

;; Custom File (for emacs to fill
(setq custom-file "~/.emacs.custom.el")
(load custom-file 'noerror)

;; Basic Configuration
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(set-face-attribute 'default nil :height 120)

;; Zoom step
(setq text-scale-mode-step 1.1)
(setq inhibit-startup-screen 1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(global-font-lock-mode 1)
(global-auto-revert-mode 1)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq shell-file-name "/usr/bin/zsh")

;; Kill all buffers (for my autism)
(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "C-x K") 'nuke-all-buffers)

;; (Fancy) Compile Mode
(setq compilation-always-kill t)
(setq compile-command "")

(add-to-list 'load-path "~/.emacs.d/fancy-compilation")
(use-package fancy-compilation
  :commands (fancy-compilation-mode))
(setq fancy-compilation-override-colors nil)
(setq fancy-compilation-term "dumb")
(setq fancy-compilation-quiet-prolog nil)
(setq fancy-compilation-quiet-prelude nil)

(with-eval-after-load 'compile
  (fancy-compilation-mode))


;; Shell env for compile mode
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Autocompletion for files
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching 1)

(tab-line-mode 0)
(tab-bar-mode 0)

(setq-default tab-width 2)  ;; Set the default tab width to 2 spaces

(global-tab-line-mode 0)
(setq tab-line-new-button-show nil)

;; Theme
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes/")

(load-theme 'gruber-darkest 1)

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-vibrant t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (nerd-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Make sure recent files are on
(recentf-mode 1)
(add-hook 'delete-frame-functions #'recentf-save-list)
(add-hook 'server-after-make-frame-hook #'recentf-load-list)

;(add-hook 'after-make-frame-functions 'recentf-load-list)
;(add-hook 'after-make-frame-functions 'projectile-load-known-projects)

;; Enable Many Windows for GDB
(setq gdb-many-windows t)

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (projects . 10)))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

(add-hook 'server-after-make-frame-hook #'dashboard-refresh-buffer)
(add-hook 'window-setup-hook
	  (lambda ()
	    (unless (daemonp)
	      (dashboard-setup-startup-hook))))

(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

;; Which key mode
(which-key-mode)

;; eglot as LSP:
;; Enable Eglot for programming modes
(use-package eglot
  :ensure t
  :hook
  ((python-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (c-mode . eglot-ensure)))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c C-r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c C-f") #'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c C-h") #'eglot-help-at-point)
  (define-key eglot-mode-map (kbd "C-c C-d") #'flymake-show-buffer-diagnostics)
  (define-key eglot-mode-map (kbd "M-.") #'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-?") #'xref-find-references)
  (define-key eglot-mode-map (kbd "M-,") #'xref-pop-marker-stack)
  (define-key eglot-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key eglot-mode-map (kbd "M-p") #'flymake-goto-prev-error))

(setq eglot-extend-to-xref t) ; Use Eglot for cross-references
(setq eglot-autoshutdown t)  ; Automatically stop server when no more buffers

(require 'flycheck)
(add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1) (flycheck-mode 1)))

;; projectile
(projectile-mode +1)
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(projectile-register-project-type 'noob '("noob.c")
                                  :project-file "noob.c"
				  :compile "./noob"
				  :test "./noob test"
				  :run "./noob run")

;; mode - packages
(require 'rust-mode)

(use-package go-mode
  :ensure t
  :hook (before-save . gofmt-before-save)  ;; Format before saving
  :config
  (setq gofmt-command "goimports"))

;; magit
(use-package magit
  :ensure t)

(global-set-key (kbd "C-x g") 'magit-status)

;; evil setup
(use-package evil
  :ensure t
  :config
	(setq evil-undo-system 'undo-redo)
  (evil-mode 1))

(evil-mode 1)

(defun my/setup-evil ()
       (evil-mode 1))

(add-hook 'window-startup-hook 'my/setup-evil)

(define-key evil-window-map (kbd "<left>") 'evil-window-left)
(define-key evil-window-map (kbd "<right>") 'evil-window-right)
(define-key evil-window-map (kbd "<up>") 'evil-window-up)
(define-key evil-window-map (kbd "<down>") 'evil-window-down)
(evil-collection-init)


;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("M-<down>" . mc/mark-next-like-this)  
         ("M-<up>" . mc/mark-previous-like-this))) 

;; Org Mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; Emacs Mode for mini
(define-derived-mode mini-mode prog-mode "mini"
  "A simple major mode for mini."

  ;; Define Keywords for highlighting
  (setq mini-font-lock-keywords
	'(("\\<\\(if\\|else\\|match\\|let\\|in\\|int\\)\\>" . font-lock-keyword-face) ;; keywords
	  ("#\\(.\\)*?#" . font-lock-comment-face) ;; multi-line comments delimited by #
          ("#.*" . font-lock-comment-face))) ;; single-line comments starting with #

  ;; Apply the syntax highlighting
  (setq font-lock-defaults '((mini-font-lock-keywords))))

;; Associate the mode with .mini files
(add-to-list 'auto-mode-alist '("\\.mi\\'" . mini-mode))

;; For remote
(setq tramp-default-method "ssh")
(defun rechnerhalle-ssh ()
  (interactive)
  (dired "/limbu@lxhalle.in.tum.de:/home/limbu/"))
