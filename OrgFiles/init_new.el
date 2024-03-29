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

;set doom themes
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))
				      ;use doom mode
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config 
  (setq doom-modeline-height 15))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :init (which-key-mode)
  :diminish (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;;;;; Org mode setup ;;;;;

					;require tempo

(defun org-mode-setup()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))


(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-agenda-files
	'("~/Projects/emacsone/OrgFiles/tasks.org"
	  "~/Projects/emacsone/OrgFiles/habits.org"))
	
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)
  (setq org-capture-babel-evaluate t)
					; org capture
  
  (setq org-capture-templates
	'(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/Projects/emacsone/OrgFiles/tasks.org" "Inbox")
	   "* TODO %?\n %U\n %a\n %i" :empty-lines 1)
	  ("ts" "Clockked Entry Subtask" entry (clock)
	   "* TODO %?\n %U\n %a\n %i" :empty-lines 1)
	  
	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
	   (file+olp+datetree "~/Projects/emacsone/OrgFiles/journal.org")
	   "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
	   ;;
	   :clock-in :clock-resume
	   :empty-lines 1)
	  ("jm" "Meeting" entry
	   (file+olp+datetree "~/Projects/emacsone/OrgFiles/journal.org")
	   "* %<%I:%M %P> - %a :meetings:\n\n%?\n\n"
	   :clock-in :clock-resume
	   :empty-lines 1)

	  ("w" "Workflows")
	  ("we" "Checking Email" entry (file+olp+datetree "~/Projects/emacsone/OrgFiles/journal.org")
	   "* Checking Email :email:\n\n%?" :clockin :clock-resume :empty-lines 1)

	  ("m" "Metrics Capture")
	  ("mw" "Weight" table-line (file+headline "~/Projects/emacsone/OrgFiles/metrics.org" "Weight")
	   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t))))

					; hotkey bindings
  (define-key global-map (kbd "C-c o")
    (lambda () (interactive) (org-capture)))

					; refile targets
  

  (setq org-refile-targets
	'(("archive.org" :maxlevel . 1)
	  ("tasks.org" :maxlevel . 1)))
					; load org habits
  (require 'org-habit)
   (add-to-list 'org-modules 'org-habit)
   (setq org-habit-graph-column 60)
					


;;;;; end org mode setup ;;;;;

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t )
   (python .t)))

;;auto tangle my emacs config file
(defun emacsone/org-babel-tangle-config()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/Projects/emacsone/OrgFiles/emacsconf.org"))
    ;; dynamic scoping
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'emacsone/org-babel-tangle-config)))

(defun org-mode-visual-fill()
  (setq visual-fill-column-width 175
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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

(use-package rainbow-delimiters

  :hook (prog-mode . rainbow-delimiters-mode))
