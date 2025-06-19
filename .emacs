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

(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)
(setq-default standard-indent 2)

(global-set-key (kbd "C-#") 'comment-region)

;; Dont show DOS file ending
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (setq-local buffer-display-table (make-display-table))
            (aset buffer-display-table ?\^M [])))

;; Zoom step
(setq text-scale-mode-step 1.1)
(setq inhibit-startup-screen 1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(global-font-lock-mode 1)
(global-auto-revert-mode 1)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq shell-file-name "/usr/bin/zsh")

;; Window Rotations
(defun rotate-vertical-to-horizontal ()
  "Switch the current vertical split layout to horizontal split."
  (interactive)
  (let ((buffer1 (window-buffer (next-window)))
        (buffer2 (window-buffer (selected-window))))
    (delete-window (next-window))  ; Close the right window in a vertical split
    (split-window-horizontally)    ; Create a horizontal split
    (set-window-buffer (selected-window) buffer1)  ; Reuse the buffer in the original window
    (set-window-buffer (next-window) buffer2)))    ; Reuse the buffer in the new window

(defun rotate-horizontal-to-vertical ()
  "Switch the current horizontal split layout to vertical split."
  (interactive)
  (let ((buffer1 (window-buffer (next-window)))
        (buffer2 (window-buffer (selected-window))))
    (delete-window (next-window))  ; Close the bottom window in a horizontal split
    (split-window-vertically)     ; Create a vertical split
    (set-window-buffer (selected-window) buffer1)  ; Reuse the buffer in the original window
    (set-window-buffer (next-window) buffer2)))    ; Reuse the buffer in the new window

(global-set-key (kbd "C-x 4 v") 'rotate-vertical-to-horizontal)
(global-set-key (kbd "C-x 4 h") 'rotate-horizontal-to-vertical) 

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

(use-package rainbow-mode)
(rainbow-mode)

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

;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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
	:config
	(add-to-list 'eglot-server-programs '((csharp-mode) . ("/opt/omni/OmniSharp" "-lsp")))
  :hook
  ((python-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (c-mode . eglot-ensure)
	 (csharp-mode . eglot-ensure)))

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

(use-package company
  :init
  (global-company-mode)
	(setq company-idle-delay nil)
  :bind
  ("C-c ." . company-complete))

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
(setq evil-undo-system 'undo-redo)
(setq undo-auto-amalgamate nil)
(setq evil-want-fine-undo t)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(define-key evil-window-map (kbd "<left>") 'evil-window-left)
(define-key evil-window-map (kbd "<right>") 'evil-window-right)
(define-key evil-window-map (kbd "<up>") 'evil-window-up)
(define-key evil-window-map (kbd "<down>") 'evil-window-down)

(dolist (key '("<left>" "<right>" "<up>" "<down>"))
  (define-key evil-normal-state-map (kbd key) nil)
  (define-key evil-motion-state-map (kbd key) nil)
  (define-key evil-operator-state-map (kbd key) nil))

(evil-collection-init)

(use-package evil-multiedit)
(evil-multiedit-default-keybinds)

;; Org Mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)

;; modes for my languages
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(require 'klaus-mode)
;; (require 'mini-mode)

;; GLSL Mode
(require 'glsl-mode)
(setq auto-mode-alist
			(append auto-mode-alist
							'(("\\.glsl\\'" . glsl-mode)
								("\\.fs\\'" . glsl-mode)
								("\\.vs\\'" . glsl-mode))))

;; For remote
(setq tramp-default-method "ssh")
(defun rechnerhalle-ssh ()
  (interactive)
  (dired "/limbu@lxhalle.in.tum.de:/home/limbu/"))

;; Markdown Live Preview using Impatient Mode
