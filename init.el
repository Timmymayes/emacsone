(fset 'encode-utf-8
   (kmacro-lambda-form [?\M-x ?r ?e ?v ?e ?r ?t ?- ?b ?u ?f ?f ?e ?r ?- ?w ?i ?t ?h ?- ?c ?o ?d tab return ?u ?t ?f ?- ?8 return ?y ?e ?s return] 0 "%d"))
(global-set-key (kbd "C-c x e") 'encode-utf-8)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;; old org link commented out for deprication 
                         ;;("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")

                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
;; clean up org package issues
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms

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

(setq
 user-mail-address "Timmymayes@gmail.com"
 user-full-name "Timothy Tyler Mayes"
 mu4e-compose-signature
 (concat
  "Best Regards,\n"
  "Tyler Mayes"))
;; Not working atm
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "Timmymayes@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq inhibit-startup-message t) ;inhibit start up
(scroll-bar-mode -1)   ;Disable visible scrollbar
(tool-bar-mode -1)     ;DIsable toolbar
(tooltip-mode -1)      ;disable tooltips11
(set-fringe-mode 10)   ;give some breathing room
(menu-bar-mode -1)     ;disable menu bar

(setq visible-bell t)  ; setup the visible bell

(use-package hydra)



(set-face-attribute
 'default nil :font "Fira Code Retina" :height 140)  ; set font
(load-theme 'tango-dark)                             ; load theme
;;(desktop-save-mode 1)                                ; enable desktop saving

;;set doom themes
(use-package doom-themes
  :ensure t
  :config
  ;;(load-theme 'doom-gruvbox t))
  (load-theme 'doom-sourcerer t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  ;; consider adding a mu4e alert
  (setq doom-modeline-mu4e t)
  (mu4e-alert-enable-mode-line-display)
  (setq doom-modeline-height 15))

;; display time
(display-time-mode)

(set-frame-parameter (selected-frame) 'alpha '(90 . 60))
(add-to-list 'default-frame-alist '(alpha . (90 . 60)))

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
         '(90 . 60) '(100 . 100)))))
(global-set-key (kbd "C-c x t") 'toggle-transparency)
;; testing if this works to set transparency to full on startup
(toggle-transparency)

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
    (vertico-cycle nil)
    (vertico-count 13)
    (vertico-resize t)
    :init
    (vertico-mode))

  (use-package savehist
    :init
    (savehist-mode))

  (use-package marginalia
    :after vertico
    :ensure t
    :custom
    (marginalia-max-relative-age 0)
    (marginalia-align 'center)
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))

  ;; turn on all the icons for completions
(use-package all-the-icons-completion
  :after(marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(yas-global-mode 1)

(defhydra hydra-zoom (global-map "<f17>")
  "Zoom"
  ("+" text-scale-increase "in")
  ("=" text-scale-decrease))

(use-package ace-window)
(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 3.0)))))
;;

(use-package avy
  :config (progn
            (setq avy-background nil)
            (setq avy-styles-alist '((avy-goto-char-2 . at)
                                     (avy-goto-char-timer . at)))))




(global-set-key (kbd "M-.") 'avy-goto-char-2)
(global-set-key (kbd "M-,") 'avy-goto-char-timer)
;; unbund c-] from abort-recursive-edit
(global-set-key (kbd "C-+") 'smartscan-symbol-go-backward)
(global-set-key (kbd "C-=") 'smartscan-symbol-go-forward)

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
  (setq org-startup-with-inline-images t)
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)
  (setq org-capture-babel-evaluate t)
  (setq org-startup-with-inline-images t)

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


(global-set-key (kbd "s-i") 'org-toggle-inline-images)  


                                        ; refile targets


(setq org-refile-targets
      '(("archive.org" :maxlevel . 1)
        ("tasks.org" :maxlevel . 1)))
                                        ; load org habits
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(add-to-list  'org-src-lang-modes '("plantuml" . plantuml))

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
   (js .t)
   (dot . t)
   (plantuml . t)
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
  ( org-agenda-todo-list-sublevels nil)        

  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n i" . org-id-get-create)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n r" . org-roam-ref-add)
   ("C-c n x a" . org-roam-alias-remove)
   ("C-c n x r" . org-roam-ref-remove)
   ("C-c n x t" . org-roam-tag-remove)

   ("C-c n I" . org-roam-node-insert-immediate)
   :map org-mode-map
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


(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:15}" 'face 'org-tag)))

(use-package org-roam-ui
  :bind ("s-r" . org-roam-ui-open)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1)
  :bind (
         :map ledger-mode-map
         ("s-n" . ledger-navigate-next-uncleared)
         ("s-p" . ledger-navigate-previous-uncleared))
  :mode "\\.dat\\'")

(setq ledger-reports
      '(("bal"            "%(binary) -f %(ledger-file) bal")
        ("bal this month" "%(binary) -f %(ledger-file) bal -p %(month) -S amount")
        ("bal this year"  "%(binary) -f %(ledger-file) bal -p 'this year'")
        ("net worth"      "%(binary) -f %(ledger-file) bal Assets Liabilities")
        ("account"        "%(binary) -f %(ledger-file) reg %(account)")))

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
  :bind (
         :map emmet-mode-keymap
         ("M-n" . emmet-next-edit-point)
         ("M-p" . emmet-prev-edit-point)))
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
  ;; (setq-local flycheck-disabled-checkers (cl-union flycheck-disabled-checkers
  ;;                                                  '(javascript-jshint))) ; jshint doesn't work for JSX
  (show-paren-mode 1)
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
(add-hook 'rjsx-mode-hook 'emmet-mode)


(with-eval-after-load 'flycheck
  (flycheck-add-next-checker 'javascript-eslint '(t . javascript-jscs)))

;; This isn't really a package, it just provides a `haxe-mode' to work with
(use-package haxe-mode
  :mode ("\\.hx\\'" . haxe-mode)
  :no-require t
  :init
  (require 'js)
  (define-derived-mode haxe-mode js-mode "Haxe"
    "Haxe syntax highlighting mode. This is simply using js-mode for now."))

(use-package battle-haxe
  :hook (haxe-mode . battle-haxe-mode)
  :bind (("S-<f4>" . #'pop-global-mark) ;To get back after visiting a definition
         :map battle-haxe-mode-map
         ("<f5>" . #'battle-haxe-goto-definition)
         ("<f12>" . #'battle-haxe-helm-find-references))
  :custom
  (battle-haxe-yasnippet-completion-expansion t "Keep this if you want yasnippet to expand completions when it's available.")
  (battle-haxe-immediate-completion nil "Toggle this if you want to immediately trigger completion when typing '.' and other relevant prefixes."))

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
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))  

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Company mode for yas
(global-set-key (kbd "<C-tab>") 'company-yasnippet)
                                        ;  (use-ackage company-box
                                        ;   :hook (company-mode . company-box-mode))



(defun next-tag()
  (interactive)
  (web-mode-element-next)
  (web-mode-tag-end))



(global-set-key  (kbd "C-x t") 'next-tag)

;; timer controls
(global-set-key (kbd "H-t t") 'org-timer-set-timer)
(global-set-key (kbd "H-t s") 'org-timer-start)
(global-set-key (kbd "H-t x") 'org-timer-stop)
(global-set-key (kbd "H-t z") 'org-timer-pause-or-continue)


;; set ctrl z to undo
(global-set-key (kbd "C-z") 'undo)

;; Macros & commands
(fset 'buffer-quick-switch
      (kmacro-lambda-form [?\C-x ?b return] 0 "%d"))

;; Bindings
(global-set-key (kbd "M-+") 'other-window)
(global-set-key (kbd "M-[") 'ace-window)
(global-set-key (kbd "M-]") 'treemacs-select-window)
(global-set-key (kbd "C-c s t") 'treemacs)
(global-set-key (kbd "H-<escape>") 'delete-window)
(global-set-key (kbd "H-1") 'delete-other-windows)
(global-set-key (kbd "H-2") 'split-window-below)
(global-set-key (kbd "H-3") 'split-window-right)
(global-set-key (kbd "C-H-b") 'buffer-menu)
(global-set-key (kbd "H-b") 'buffer-quick-switch)
(global-set-key (kbd "C-H-g") 'list-bookmarks)
(global-set-key (kbd "C-H-t") 'dired-jump)
(global-set-key (kbd "H-k") 'kill-current-buffer)

(defvar active-harpoon)
(setq active-harpoon 102)

(defun current-buffer-is-harpooned (marker)
  (and (eq (marker-buffer marker) (current-buffer))))  

(defun harpoon-f ()
  "Update point if in an a harpooned register and jump to the point harpooned in the 'f' register."
  (interactive)
  (if (current-buffer-is-harpooned (get-register active-harpoon)) (point-to-register active-harpoon))
  (jump-to-register 102)
  (setq active-harpoon 102))

(defun set-harpoon-f ()
  "Harpoon the current buffer in the 'f' register"
  (interactive)
  (point-to-register 102)
  (setq active-harpoon 102)    
  )

(defun harpoon-d ()
  "Update point if in an a harpooned register and jump to the point harpooned in the 'd' register."    
  (interactive)
  (if (current-buffer-is-harpooned (get-register active-harpoon)) (point-to-register active-harpoon))
  (jump-to-register 100)
  (setq active-harpoon 100))

(defun set-harpoon-d ()
  "Harpoon the current buffer in the 'd' register"
  (interactive)
  (point-to-register 100)
  (setq active-harpoon 100)    
  )

(defun harpoon-a ()
  "Update point if in an a harpooned register and jump to the point harpooned in the 'a' register."    
  (interactive)
  (if (current-buffer-is-harpooned (get-register active-harpoon)) (point-to-register active-harpoon))
  (jump-to-register 97)
  (setq active-harpoon 97))

(defun set-harpoon-a ()
  "Harpoon the current buffer in the 'a' register"
  (interactive)
  (point-to-register 97)
  (setq active-harpoon 97)    
  )

(defun harpoon-s ()
  "Update point if in an a harpooned register and jump to the point harpooned in the 'f' register."
  (interactive)
  (if (current-buffer-is-harpooned (get-register active-harpoon)) (point-to-register active-harpoon))
  (jump-to-register 115)
  (setq active-harpoon 115))

(defun set-harpoon-s ()
  "Harpoon the current buffer in the 's' register"    
  (interactive)
  (point-to-register 115)
  (setq active-harpoon 115)
  )

(global-set-key (kbd "H-a") 'harpoon-a)
(global-set-key (kbd "s-a") 'set-harpoon-a)
(global-set-key (kbd "H-s") 'harpoon-s)
(global-set-key (kbd "s-s") 'set-harpoon-s)
(global-set-key (kbd "H-d") 'harpoon-d)
(global-set-key (kbd "s-d") 'set-harpoon-d)
(global-set-key (kbd "H-f") 'harpoon-f)
(global-set-key (kbd "s-f") 'set-harpoon-f)

(defun my/pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer, repeat calls will cycle"
  (interactive)
  (set-mark-command t))

(defun my/pop-global-mark-ring()
  "move cursor to last mark in global ring, repeat calls will cycle"
  (interactive)
  (pop-global-mark)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "m") 'my/pop-global-mark-ring)
     map))
  )


(defun my/insert-line-above-and-go ()
  ;;insert a line above the current one and move the cursor there
  (interactive)
  (previous-line nil)
  (move-end-of-line nil)
  (electric-newline-and-maybe-indent)
  (indent-relative-first-indent-point))


(defun wrap-sexp-backward-with-parenthesis()
  "wrap the current expression backwards with parenthesis"
  (interactive)
  (backward-sexp)
  (mark-sexp) 
  (insert-parentheses))

(global-set-key (kbd "M-o") 'my/insert-line-above-and-go)
;; move C-j to C-; indent-new-comment-line
(global-set-key (kbd "C-;") 'indent-new-comment)                      
(global-set-key (kbd "H-]") 'xref-find-references)
(global-set-key (kbd "H-[") 'xref-go-back)
(global-set-key (kbd "H-g") 'goto-line)
(global-set-key (kbd "C-(") 'wrap-sexp-backward-with-parenthesis)
;; swap point and mark
(global-set-key (kbd "M-m")  (kmacro-lambda-form [?\C-u ?\C-x ?\C-x] 0 "%d"))
;; cycle marks
(global-set-key (kbd "H-m") 'my/pop-local-mark-ring)
(global-set-key (kbd "C-H-m") 'my/pop-global-mark-ring)
;; rebind back-to-indentation to "M-i" NOTE this unbinds!! tab-to-tab-stop
(global-set-key (kbd "M-i") 'back-to-indentation)

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

(defun kill-word-at-point()
  "Kill the full word at point"
  (interactive)
  (kill-word 1)
  (backward-kill-word 1))

(defun kill-line-at-point()
  "Kill full lilne at point"
  asdfsafd  adf asdas asdfasdf
  (interactive)
  (back-to-indentation)
  (kill-line))

(global-set-key (kbd "C-M-i") 'indent-region)
(global-set-key (kbd "M-DEL") 'kill-word-at-point)
(global-set-key (kbd "M-k") 'kill-line-at-point)
(global-set-key (kbd "s-k") 'kill-sentence)
(global-set-key (kbd "s-y") 'duplicate-current-line)
(global-set-key (kbd "H-y") 'duplicate-line-up-to-point)

; list directories first
(setq dired-listing-switches "-agho --group-directories-first")
(setq dired-dwim-target t)

;;  (use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(defun my-dired-mode-hook ()
  "My `dired' mode hook."
  ;; To hide dot-files by default
  (dired-hide-dotfiles-mode))

;; To toggle hiding
(define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
(add-hook 'dired-mode-hook #'my-dired-mode-hook)

;;set load path for person elisp
(add-to-list 'load-path "~/.emacs.d/lisp")
;; Removing iy-go-to-char
                                       ;load the package iy-go-to-char
                                       ;(load "iy-go-to-char")
 ;;Unbind C-m from return  

 ;; eventually make this your first fully functional lisp
 (fset 'yank-and-add-line-numbers
       (kmacro-lambda-form [?\C-x ?r ?N ?\C-x ?\C-x ?÷ ?\C-z] 0 "%d"))
 (global-set-key (kbd "H-s-k") 'yank-and-add-line-numbers) 


 (fset 'agenda-fullscreen
       (kmacro-lambda-form [?\C-c ?a ?a ?\C-x ?1] 0 "%d"))

(fset 'agenda-with-tasks-fullscreen
 (kmacro-lambda-form [?\C-c ?a ?n ?\H-1] 0 "%d"))


 (global-set-key (kbd "<f13>") 'agenda-fullscreen)
 (global-set-key (kbd "<f14>") 'agenda-with-tasks-fullscreen)
(global-set-key (kbd "<f6>") 'browse-url-of-buffer)

;;Calendar keys
(global-set-key (kbd "s-c") 'calendar)
(global-set-key (kbd "C-H-d") 'magit-dispatch)

;; open

(defun play-retro-sax ()
    "Launch a retro sax 3 hour video"
  (interactive)
  (browse-url "https://www.youtube.com/watch?v=001hXHNo-3w"))

(defun play-high-bpm-trance ()
  "Launch high bpm trance video"
  (interactive)
  (browse-url "https://youtu.be/q7gv9B4Kw44"))


(defhydra Music (global-map "<f7>")
  "Music Player"
  ("s" play-retro-sax "Retro-Sax")
  ("h" play-high-bpm-trance "High BPM Trance"))

(fset 'my/dired-create-game-design-structure
     (kmacro-lambda-form [?\M-x ?m ?k ?d ?i ?r return ?d ?e ?s ?i ?g ?n return ?\M-x ?m ?d ?k backspace backspace ?k ?d ?i ?r return ?d ?e ?v ?e ?l ?o ?p ?m ?e ?n ?t return ?\M-x ?m ?k ?d ?i ?r return ?d ?o ?c ?u ?m ?e ?n ?t ?a ?t ?i ?o ?n return ?\M-x ?m ?k ?d ?i ?r return ?i ?m ?a ?g ?e ?s return ?\M-x ?m ?k ?d ?i ?r ?  ?p ?l ?a ?y ?t ?e ?s ?t ?s return backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace return ?p ?l ?a ?y ?t ?e ?s ?t ?s return] 0 "%d"))

(fset 'my/dired-populate-design-docs
   (kmacro-lambda-form [?\C-x ?\C-f ?R ?E ?A ?D ?M ?E ?. ?o ?r ?g return ?\C-x ?\C-s ?\H-k ?g ?\C-x ?\C-f ?d ?e ?s ?i ?g ?n ?/ ?D ?e ?s ?i ?g ?n ?- ?L ?o ?g ?. ?o ?r ?g return ?\C-x ?\C-s ?\H-k ?\C-x ?\C-f ?d tab ?D ?e ?s ?i ?g backspace backspace backspace backspace backspace ?M ?a ?s ?t ?e ?r ?- ?D ?e ?s ?i ?g ?n ?- ?D ?o ?c ?. ?o ?r ?g return ?\C-x ?\C-s ?\H-k ?\C-x ?\C-f ?p ?l ?a ?y tab ?P ?l ?a ?y ?t ?e ?s ?t ?s ?. ?o ?r ?g return ?\C-x ?\C-s ?\H-k ?\C-s ?\C-f ?\C-x ?\C-f ?d ?o ?c ?u tab ?R ?u ?l ?e ?s ?. ?o ?r ?g return ?\C-x ?\C-s ?\H-k] 0 "%d"))

(fset 'my/create-new-design
 (kmacro-lambda-form [?\M-x ?m ?y ?/ return ?g ?\M-x ?m ?y ?/ ?\C-n ?\C-n ?\C-n ?\C-n return ?g] 0 "%d"))

(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous block.

• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.
• if `visual-line-mode' is on, beginning of line means visual line.

URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version: 2018-06-04 2021-03-16 2022-03-30 2022-07-03 2022-07-06"
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (eq last-command this-command))
        (when
            (re-search-backward "\n[\t\n ]*\n+" nil 1)
          (skip-chars-backward "\n\t ")
          (forward-char))
      (if visual-line-mode
          (beginning-of-visual-line)
        (if (eq major-mode 'eshell-mode)
            (progn
              (declare-function eshell-bol "esh-mode.el" ())
              (eshell-bol))
          (back-to-indentation)
          (when (eq $p (point))
            (beginning-of-line)))))))


(defun xah-end-of-line-or-block ()
  "Move cursor to end of line or next block.

• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.
• if `visual-line-mode' is on, end of line means visual line.

URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version: 2018-06-04 2021-03-16 2022-03-05"
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command))
      (re-search-forward "\n[\t\n ]*\n+" nil 1)
    (if visual-line-mode
        (end-of-visual-line)
      (end-of-line))))

(global-set-key (kbd "M-p") 'xah-beginning-of-line-or-block)
(global-set-key (kbd "M-n") 'xah-end-of-line-or-block)

;;(desktop-read)

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(let ((table '(("a" "Hello") ("b" "World!"))))
(mapcar #'(lambda (x)
            (princ (format "%s [label =\"%s\", shape = \"box\"];\n"
                           (first x) (second x)))) table)
(princ (format "%s -- %s;\n" (first (first table)) (first (second table))))
)
