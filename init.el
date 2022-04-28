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
;  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


					;replace dashes with dots

(font-lock-add-keywords 'org-mode
                         '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; This is needed as of Org 9.2
;  (require 'org-tempo)
 ;  (with-eval-after-load 'org-tempo
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
  (setq visual-fill-column-width 150 visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(setq org-clock-sound "~/Downloads/cheer.wav")

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(defun ar/git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name "~/Downloads/"))
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

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

(defun my-display-numbers-hook ()
  (display-line-numbers-mode t)
  )
(add-hook 'prog-mode-hook 'my-display-numbers-hook)
(add-hook 'text-mode-hook 'my-display-numbers-hook)
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

; breadcrumb setup

(defun lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)

  (use-package lsp-mode
    :commands (lsp lsp-deffered)
    :init
    (setq lsp-keymap-prefix "C-c l")
    :config
    (lsp-enable-which-key-integration t))
  ; turn on lsp ui

  (use-package lsp-ui
    :after lsp
    :hook (lsp-mode . lsp-ui-mode)
    :config
    (setq lsp-ui-doc-position 'bottom))

  (use-package lsp-treemacs
    :after lsp)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ( "<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common)) 
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;  (use-package company-box
 ;   :hook (company-mode . company-box-mode))

(use-package term
:config
(setq explicit-shesll-file-name "bash")
(setq termp-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))



; list directories first
(setq dired-listing-switches "-agho --group-directories-first")
(setq dired-dwim-target t)
