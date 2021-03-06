(fset 'encode-utf-8
   (kmacro-lambda-form [?\M-x ?r ?e ?v ?e ?r ?t ?- ?b ?u ?f ?f ?e ?r ?- ?w ?i ?t ?h ?- ?c ?o ?d tab return ?u ?t ?f ?- ?8 return ?y ?e ?s return] 0 "%d"))
(global-set-key (kbd "C-c x e") 'encode-utf-8)

(require 'package)



(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;;("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")

                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)
(unless package-archive-contents
 (package-refresh-contents))

                                        ; Initialize use-package on non-Linux platforms

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package mu4e
   :ensure nil
   ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
   :config
   ;; this si set to t to avoid mail syncing issues
   (setq mu4e-change-filenames-when-moving t)
   ;; refresh mail using isync every 10 minutes
   (setq mu4e-update-interval (* 10 60))
   (setq mu4e-get-mail-command "mbsync -a")
   (setq mu4e-maildir "~/Mail")

   (setq mu4e-drafts-folder "/'[Gmail]'.Drafts")
   (setq mu4e-sent-folder "/[Gmail].Sent Mail")
   (setq mu4e-refile-folder "/[Gmail].All Mail")
   (setq mu4e-trash-folder "/[Gmail].Trash")

   (setq mu4e-maildir-shortcuts
   '(("/Inbox"                   . ?i)
     ("/[Gmail].Sent Mail"       . ?s)
     ("/[Gmail].Trash"           . ?t)
     ("/[Gmail].Drafts"          . ?d)
     ("/[Gmail].All Mail"        . ?a))))

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

(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c x t") 'toggle-transparency)

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

;;           (use-package counsel
;;             :bind (("M-x" . counsel-M-x)
;;                    ("C-x b" . counsel-ibuffer)


;; story)))

  ;;        (use-package ivy-richt
    ;;        :init
      ;;      (ivy-rich-mode 1))

          (use-package vertico
            :ensure t
            :custom
            (vertico-cycle t)
            :init
            (vertico-mode))
  (use-package savehist
    :init
    (savehist-mode))

  (use-package marginalia
    :after vertico
    :ensure t
    :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))

(yas-global-mode 1)

(use-package ace-window)



(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 3.0)))))



(global-set-key (kbd "C-M-]") 'avy-goto-word-or-subword-1)
;; unbund c-] from abort-recursive-edit
(global-set-key (kbd "C-+") 'smartscan-symbol-go-backward)
(global-set-key (kbd "C-=") 'smartscan-symbol-go-forward)
(global-set-key (kbd "M-RET") 'counsel-ibuffer)

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
        '("~/Orgfiles/tasks.org"
          "~/Orgfiles/habits.org"))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ???"
        org-hide-emphasis-markers t)
  (setq org-capture-babel-evaluate t)
                                        ; org capture

  (setq org-capture-templates
        '(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Orgfiles/tasks.org" "Inbox")
           "* TODO %?\n %U\n %a\n %i" :empty-lines 1)
          ("ts" "Clockked Entry Subtask" entry (clock)
           "* TODO %?\n %U\n %a\n %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/Orgfiles/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;;
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/Orgfiles/journal.org")
           "* %<%I:%M %P> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Orgfiles/journal.org")
           "* Checking Email :email:\n\n%?" :clockin :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")

          ("mw" "Weight" table-line (file+headline "~/Orgfiles/metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t))))

                                        ; hotkey bindings
  (define-key global-map (kbd "C-c o")
    (lambda () (interactive) (org-capture)))

  (define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))

  (define-key global-map (kbd "C-c t")
  (lambda () (interactive) (org-capture nil "tt")))

  (global-set-key (kbd "C-c a") 'org-agenda)


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
  (org-bullets-bullet-list '("???" "???" "???" "???" "???" "???" "???")))


					;replace dashes with dots

(font-lock-add-keywords 'org-mode
                         '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "???"))))))

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

(use-package org-roam
      :ensure t
      :init
      (setq org-roam-v2-act t)
      :custom
      (org-roam-directory "~/RoamNotes")
      (org-roam-completion-everywhere t)

      :bind
      (("C-c n l" . org-roam-buffer-toggle)
       ("C-c n f" . org-roam-node-find)
       ("C-c n i" . org-roam-node-insert)
       ("C-c n c" . org-id-get-create)
       ("C-c n a" . org-roam-alias-add)
       ("C-c n r" . org-roam-ref-add)
       ("C-c n x a" . org-roam-alias-remove)
       ("C-c n x r" . org-roam-ref-remove)

       ("C-c n I" . org-roam-node-insert-immediate)
       :map org-mode-map
       ("C-M-i" . completion-at-point)
       ("C-c n b" . org-mark-ring-goto)
       :map org-roam-dailies-map
       ("Y" . org-roam-dailies-capture-yesterday)
       ("T" . org-roam-dailies-capture-tomorrow))

       :bind-keymap
       ("C-c n d" . org-roam-dailies-map)
       :config
       (require 'org-roam-dailies)
       (org-roam-db-autosync-mode))

;;  Bind this to C-c n I
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))  



      (with-eval-after-load "org-roam" 
        (setq org-roam-capture-templates
              '(("d" "default" plain
                 "%?"
                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
                 :unnarrowed t)
                ;; programming language
                ("l" "programming language" plain
                 "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
                 :unnarrowed t)
                ;; programming insight - javascript
                ("i" "Programming Insights" plain
                "* Problem\n\n* Insight:\n\n* Solution:\n\n* Refactoring:\n\n* Fig1:\n\n#+BEGIN_SRC javascript\n\n\n#+END_SRC"
                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
                 :unnarrowed t)
                ("b" "book notes" plain
                 "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\nest")
                 :unnarrowed t))))




      (use-package org-roam-ui
        :bind ("s-r" . org-roam-ui-open))

(use-package ledger-mode
:ensure t
:init
(setq ledger-clear-whole-transactions 1)
:mode "\\.dat\\'")

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("s-m m" . magit-status)
         ("s-m j" . magit-dispatch)
         ("s-m k" . magit-file-dispatch)
         ("s-m l" . magit-log-buffer-file)
         ("s-m b" . magit-blame))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(setq magit-clone-default-directory "~/Projects/")

;; Bindings

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))
(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

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
    (set-process-filter proc #'comintoutput-filter)))

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

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)
(global-set-key (kbd "C-c s (") 'electric-pair-mode)

(setq display-line-numbers-type 'relative)

(defun my-display-numbers-hook ()
  (display-line-numbers-mode t)
  )
(add-hook 'prog-mode-hook 'my-display-numbers-hook)
(add-hook 'text-mode-hook 'my-display-numbers-hook)
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'prog-mode-hook 'subword-mode)

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package minimap)

(setq minimap-window-location 1)

(global-set-key (kbd "C-c s m")  'minimap-mode)

(use-package web-mode)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)
; hook into web mode for file types
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;using rsjx mode
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
;; using rsjx mode
;;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

; set company completions vocab to css and html

(setq web-mode-enable-engine-detection t)

(use-package emmet-mode
:bind 
("M-n" . emmet-next-edit-point)
("M-p" . emmet-prev-edit-point))
; use emmet in all web-mode docs
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

; enable mode switching between css and java
(add-hook 'web-mode-before-auto-complete-hooks
    '(lambda ()
     (let ((web-mode-cur-language
  	    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
    	   (yas-activate-extra-mode 'php-mode)
      	 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
    	   (setq emmet-use-css-transform t)
      	 (setq emmet-use-css-transform nil)))))



; breadcrumb setup

(defun lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

  (use-package lsp-mode
    :commands (lsp lsp-deffered)
    :hook (lsp-mode . lsp-mode-setup)
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
  (setq treemacs-select-when-already-in-treemacs 'close)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package rjsx-mode
  :mode ("\\.js\\'"
         "\\.jsx\\'")
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2
        js-indent-level 2)
  (setq-local flycheck-disabled-checkers (cl-union flycheck-disabled-checkers
                                                   '(javascript-jshint))) ; jshint doesn't work for JSX
  (electric-pair-mode 1))

(use-package add-node-modules-path
  :defer t
  :hook (((js2-mode rjsx-mode) . add-node-modules-path)))

;; prettify

(use-package prettier-js
  :defer t
  :diminish prettier-js-mode
  :hook (((js2-mode rjsx-mode) . prettier-js-mode)))

;; setup lsp mode
(use-package lsp-mode
  :defer t
  :diminish lsp-mode
  :hook (((js2-mode rjsx-mode) . lsp))
  :commands lsp
  :config
  (setq lsp-auto-configure t
        lsp-auto-guess-root t
        ;; don't set flymake or lsp-ui so the default linter doesn't get trampled
        lsp-diagnostic-package :none))



(use-package lsp-ui
  :defer t
  :config
  (setq lsp-ui-sideline-enable t
        ;; disable flycheck setup so default linter isn't trampled
        lsp-ui-flycheck-enable nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-peek-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-doc-enable nil))


(defun my-js-comint-keys ()
  "My Keys for sending to the js-comint repl"
  (interactive)
  (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
  (local-set-key (kbd"C-c b") 'js-send-buffer)
  (local-set-key (kbd"C-c r") 'js-send-region)
  (local-set-key (kbd"C-c C-r") 'js-send-region-and-go))




(require 'js-comint)
(setq inferior-js-program-command "node --interactive")
(setenv "NODE_NO_READLINE" "1")
(add-hook 'rjsx-mode-hook 'my-js-comint-keys)


(with-eval-after-load 'flycheck
  (flycheck-add-next-checker 'javascript-eslint '(t . javascript-jscs)))

(use-package company
    :after lsp-mode
    :hook ((lsp-mode web-mode) . company-mode)
    :bind (:map company-active-map
                ( "<tab>" . company-complete-selection))
    (:map lsp-mode-map
          ("<tab>" . company-indent-or-complete-common)) 
)
    (setq company-minimum-prefix-length 2)



  (use-package company-web
:after company)


(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files))
  (html-autoview-mode 1))  

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Company mode for yas
(global-set-key (kbd "<C-tab>") 'company-yasnippet)
  ;  (use-ackage company-box
   ;   :hook (company-mode . company-box-mode))

(global-set-key (kbd "M-=") 'dabbrev-expand
                )
(global-set-key (kbd "M-C-=") 'dabbrev-completion)

(defun next-tag()
  (interactive)
    (web-mode-element-next)
    (web-mode-tag-end))



(global-set-key  (kbd "C-x t") 'next-tag)

;; set ctrl z to undo
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "M-+") 'other-window)
(global-set-key (kbd "M-[") 'ace-window)
(global-set-key (kbd "M-]") 'treemacs-select-window)
(global-set-key [(meta left)] 'windmove-left)
(global-set-key [(meta right)] 'windmove-right)
(global-set-key [(meta up)] 'windmove-up)
(global-set-key [(meta down)] 'windmove-down)
(global-set-key (kbd "C-c s t") 'treemacs)

(defun insert-line-above-and-go ()
  ;;insert a line above the current one and move the cursor there
  (interactive)
  (previous-line nil)
  (move-end-of-line nil)
  (electric-newline-and-maybe-indent)
  (indent-relative-first-indent-point))

(global-set-key (kbd "M-o") 'insert-line-above-and-go)

;;
;

; ;

;; move C-j to C-; indent-new-comment-line
(global-set-key (kbd "C-;") 'indent-new-comment-line)

(global-set-key (kbd "H-]") 'xref-find-references)
(global-set-key (kbd "H-[") 'xref-go-back)
(global-set-key (kbd "H-g") 'goto-line)

(defun wrap-sexp-backward-with-parenthesis()
  (interactive)
  (backward-sexp)
  (mark-sexp) 
  (insert-parentheses))

  (global-set-key (kbd "C-(") 'wrap-sexp-backward-with-parenthesis)


(global-set-key (kbd "s-a") 'ace-jump-word-mode)


(global-set-key (kbd "M-m")  (kmacro-lambda-form [?\C-u ?\C-x ?\C-x] 0 "%d"))

(defun kill-word-at-point()
  (interactive)
  (kill-word 1)
  (backward-kill-word 1))

  (global-set-key (kbd "M-DEL") 'kill-word-at-point)

(defun kill-line-at-point()
  (interactive)
  (back-to-indentation)
  (kill-line))
  
  (global-set-key (kbd "s-l") 'kill-line-at-point)

(defun duplicate-current-line()
  "Duplicates the entire line under point. Repetable with 'd' "
  (interactive)
  (back-to-indentation)
  (kill-line)
  (yank)
  (newline)
  (indent-for-tab-command)
  (yank)
  (set-temporary-overlay-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "d") 'duplicate-current-line)
      map)))

(defun duplicate-line-up-to-point()
 "Duplicates a line from start of indentation up to point. May be repeated with single 'd' presses."
  (interactive)
  (set-mark-command nil)
  (back-to-indentation)
  (kill-ring-save (region-beginning) (region-end))
  (end-of-line)
  (newline)
;; example of single key repeat functionality
  (yank)
  (set-temporary-overlay-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "d") 'duplicate-line-up-to-point)
      map)))


(global-set-key (kbd "H-s-d") 'duplicate-current-line
                )
(global-set-key (kbd "H-d") 'duplicate-line-up-to-point)







(progn
  ;; some Hyper keys to insert Unicode chars
  (define-key key-translation-map (kbd "H-3") (kbd "???")) ; bullet
  (define-key key-translation-map (kbd "H-4") (kbd "???")) ; white diamond
  (define-key key-translation-map (kbd "H-5") (kbd "???")) ; dagger
  )

(global-set-key (kbd "C-,") 'point-to-register)
(global-set-key (kbd "C-.") 'jump-to-register)
(global-set-key (kbd "H-s") 'bookmark-set)
(global-set-key (kbd "H-j") 'bookmark-jump)

; list directories first
(setq dired-listing-switches "-agho --group-directories-first")
(setq dired-dwim-target t)

(use-package dired-single)

(use-package all-the-icons-dired
:hook (dired-mode . all-the-icons-dired-mode))

;(use-package dired-open) look into this package if you end up needing it.

(defun my-dired-mode-hook ()
  "My `dired' mode hook."
  ;; To hide dot-files by default
  (dired-hide-dotfiles-mode))

;; To toggle hiding
(define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
(add-hook 'dired-mode-hook #'my-dired-mode-hook)

(use-package dashboard
:ensure t
:config
(dashboard-setup-startup-hook))

;;set load path for person elisp
  (add-to-list 'load-path "~/.emacs.d/lisp")

  ;; load the package iy-go-to-char
  (load "iy-go-to-char")
  ;; rebind back-to-indentation to "M-i" NOTE this unbinds!! tab-to-tab-stop
  (global-set-key (kbd "M-i") 'back-to-indentation)
  ;; rebind "M-m" iy-go-to-char
  (global-set-key (kbd "s-n") 'iy-go-to-char)
  ;;unbind C-m from return  
  (global-set-key (kbd "s-h") 'iy-go-up-to-char)
  (global-set-key (kbd "s-b") 'iy-go-to-char-backward)
  (global-set-key (kbd "s-g") 'iy-go-up-to-char-backward)

  ;; Line to copy - start with a macro
  ;; eventually make this your first fully functional lisp
  (fset 'yank-and-add-line-numbers
   (kmacro-lambda-form [?\C-x ?r ?N ?\C-x ?\C-x ??? ?\C-z] 0 "%d"))
  (global-set-key (kbd "s-k") 'yank-and-add-line-numbers) 

asdf
