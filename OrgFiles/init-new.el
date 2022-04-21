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
