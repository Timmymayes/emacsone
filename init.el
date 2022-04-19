(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;Disable visible scrollbar
(tool-bar-mode -1)   ;DIsable toolbar
(tooltip-mode -1)    ;disable tooltips11
(set-fringe-mode 10) ;give some breathing room

(menu-bar-mode -1)   ;disable menu bar

					; setup the visible bell

(setq visible-bell t)

					;set font

(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)

					; load theme note this passes an object denoted by a leading `

(load-theme 'tango-dark)
					; keybind mode specific example

(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)



				       

(require 'package)



(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

					; Initialize use-package on non-Linux platforms

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


					;  programming mode

(defun my-display-numbers-hook ()
  (display-line-numbers-mode t)
  )
(add-hook 'prog-mode-hook 'my-display-numbers-hook)
(add-hook 'text-mode-hook 'my-display-numbers-hook)
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

					; setup command log mode which shows typed commands in a window to the right	  
(use-package command-log-mode)

					;setup swiper ( better buffer)

(use-package swiper)

					;setup ivy which is fuzzy search

(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config
  (ivy-mode 1))

					;counsel setup


(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))




					; doom mode line ( imporve the mode line)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config 
  (setq doom-modeline-height 15))
					;setup rainbow delims for nesting

(use-package rainbow-delimiters

  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

					; Projectile - helps to manage projects in emacs
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


					;TODO (setup Forge)
					;https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
					;https://youtu.be/INTu30BHZGk?t=3221

;(use-package forge)

;;;;; Org mode setup ;;;;;

(defun efs/org-mode-setup()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-agenda-files
	'("~/Projects/emacsone/OrgFiles/tasks.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


					;replace dashes with dots

(font-lock-add-keywords 'org-mode
                         '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))			   

					;change fonts to be more readable

(dolist (face '((org-level-1 . 1.2)
		(org-level-2 . 1.1)
		(org-level-3 . 1.05)
		(org-level-4 . 1.0)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
					; keep a few things fixed pitch as they should be for line ups

(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

					; visual fill mode
(defun org-mode-visual-fill()
  (setq visual-fill-column-width 175
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-


