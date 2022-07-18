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
(desktop-save-mode 1)
(setq desktop-path '("~/Projects/emacsone"))
(desktop-read)

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
