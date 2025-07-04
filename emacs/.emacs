(message "Loading .emacs")

(setq custom-file "~/.emacs.custom.el")
(load custom-file 'noerror)

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(set-face-attribute 'default nil :height 120)
(setq-default indent-tabs-mode nil tab-width 2 standard-indent 2)

(use-package transpose-frame :ensure t)
(global-set-key (kbd "C-#") 'comment-region)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (setq-local buffer-display-table (make-display-table))
            (aset buffer-display-table ?\^M [])))

(setq text-scale-mode-step 1.1
      inhibit-startup-screen t
      display-line-numbers-type 'relative)

(global-display-line-numbers-mode 1)
(global-font-lock-mode 1)
(global-auto-revert-mode 1)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq shell-file-name "/usr/bin/zsh")

(defun rotate-vertical-to-horizontal ()
  (interactive)
  (let ((buffer1 (window-buffer (next-window)))
        (buffer2 (window-buffer (selected-window))))
    (delete-window (next-window))
    (split-window-horizontally)
    (set-window-buffer (selected-window) buffer1)
    (set-window-buffer (next-window) buffer2)))

(defun rotate-horizontal-to-vertical ()
  (interactive)
  (let ((buffer1 (window-buffer (next-window)))
        (buffer2 (window-buffer (selected-window))))
    (delete-window (next-window))
    (split-window-vertically)
    (set-window-buffer (selected-window) buffer1)
    (set-window-buffer (next-window) buffer2)))

(global-set-key (kbd "C-x 4 v") 'rotate-vertical-to-horizontal)
(global-set-key (kbd "C-x 4 h") 'rotate-horizontal-to-vertical)

(setq compilation-always-kill t compile-command "")

(add-to-list 'load-path "~/.emacs.d/fancy-compilation")
(use-package fancy-compilation
  :commands (fancy-compilation-mode))
(setq fancy-compilation-override-colors nil)
(setq fancy-compilation-term "dumb")
(setq fancy-compilation-quiet-prolog nil)
(setq fancy-compilation-quiet-prelude nil)

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(use-package rainbow-mode
  :config (rainbow-mode))

(when (daemonp)
  (use-package exec-path-from-shell
    :config (exec-path-from-shell-initialize)))

(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching 1)

(tab-line-mode 0)
(tab-bar-mode 0)
(global-tab-line-mode 0)
(setq tab-line-new-button-show nil)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'gruber-darkest t)

(recentf-mode 1)
(add-hook 'delete-frame-functions #'recentf-save-list)
(add-hook 'server-after-make-frame-hook #'recentf-load-list)

(setq gdb-many-windows t)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5) (projects . 10))
        dashboard-startup-banner 'logo
        dashboard-set-heading-icons t
        dashboard-set-file-icons t))

(add-hook 'server-after-make-frame-hook #'dashboard-refresh-buffer)
(add-hook 'window-setup-hook
          (lambda ()
            (unless (daemonp)
              (dashboard-setup-startup-hook))))

(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

(use-package which-key
  :config (which-key-mode))

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (csharp-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '((csharp-mode) . ("/opt/omni/OmniSharp" "-lsp")))
  (setq eglot-extend-to-xref t eglot-autoshutdown t)
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

(use-package company
  :bind ("C-c ." . company-complete)
  :config
  (global-company-mode)
  (setq company-idle-delay nil))

(use-package flycheck
  :hook (eglot-managed-mode . (lambda () (flymake-mode -1) (flycheck-mode 1))))

(use-package projectile
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-register-project-type 'noob '("noob.c")
                                    :project-file "noob.c"
                                    :compile "./noob"
                                    :test "./noob test"
                                    :run "./noob run"))

(use-package rust-mode)
(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :config (setq gofmt-command "goimports"))

(use-package ein
  :ensure t
  :config
  (setq ein:jupyter-default-notebook-directory "~/notebooks/"))

(use-package magit
  :bind ("C-x g" . magit-status))

(setq evil-undo-system 'undo-redo
      undo-auto-amalgamate nil
      evil-want-fine-undo t)

(use-package evil
  :config
  (evil-mode 1)
  (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  (define-key evil-window-map (kbd "<down>") 'evil-window-down)
  (add-hook 'ein:notebook-mode-hook #'evil-normal-state)
  (add-hook 'ein:worksheet-mode-hook #'evil-normal-state)
  (add-hook 'ein:log-mode-hook #'evil-normal-state)
  (add-hook 'ein:codecell-mode-hook #'evil-normal-state)
  (dolist (key '("<left>" "<right>" "<up>" "<down>"))
    (define-key evil-normal-state-map (kbd key) nil)
    (define-key evil-motion-state-map (kbd key) nil)
    (define-key evil-operator-state-map (kbd key) nil)))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-multiedit
  :config (evil-multiedit-default-keybinds))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.js\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))

(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.fs\\'" . glsl-mode)
         ("\\.vs\\'" . glsl-mode)))

(setq tramp-default-method "ssh")
(defun rechnerhalle-ssh ()
  (interactive)
  (dired "/limbu@lxhalle.in.tum.de:/home/limbu/"))

