;;; init.el --- Where all the magic begins

;;; Commentary:
;; https://github.com/Bassmann/emacs-config

;;; Code:
;;
;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in literate Org-mode files.

;; Avoid garbage collection during startup. The GC eats up quite a bit
;; of time, easily doubling the startup time. The trick is to turn up
;; the memory threshold in order to prevent it from running during
;; startup.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; Every file opened and loaded by Emacs will run through this list to
;; check for a proper handler for the file, but during startup, it
;; won’t need any of them.
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; After Emacs startup has been completed, set `gc-cons-threshold' to
;; 16 MB and reset `gc-cons-percentage' to its original value.  Also
;; reset `file-name-handler-alist'
(add-hook 'emacs-startup-hook
          '(lambda ()
             (setq gc-cons-threshold (* 16 1024 1024)
                   gc-cons-percentage 0.1
                   file-name-handler-alist file-name-handler-alist-original)
             (makunbound 'file-name-handler-alist-original)))

;; It may also be wise to raise gc-cons-threshold while the minibuffer
;; is active, so the GC doesn’t slow down expensive commands (or
;; completion frameworks, like helm and ivy).
(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy
  ;; the benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold (* 16 1024 1024)))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

;; I don't need the big icons and prefer more screen real estate. See
;; also https://sites.google.com/site/steveyegge2/effective-emacs
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Set repositories
(require 'package)
(setq-default load-prefer-newer t)
;; I want orgmode before melpa or gnu
(setq package-archives
      '(("ORG" . "https://orgmode.org/elpa/")
        ("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("ORG" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(when (version< emacs-version "27.0") (package-initialize))
;; Install dependencies
(unless (and (package-installed-p 'delight)
             (package-installed-p 'bind-key)
             (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'delight t)
  (package-install 'bind-key t)
  (package-install 'use-package t))

(setq-default
 use-package-always-defer t
 use-package-always-ensure t
 use-package-compute-statistics t
 use-package-verbose t)

;; https://www.emacswiki.org/emacs/GnuPG#toc2
;; (setq package-check-signature nil)
(let ((my/old package-check-signature))
  (use-package gnu-elpa-keyring-update
    :ensure t
    :init   (setq package-check-signature nil)
    :config (setq package-check-signature my/old)))

;; Use latest Org and pin
(use-package org
  :ensure org-plus-contrib
  :custom
  (org-export-backends '(ascii html md))
  :pin ORG)

(use-package auto-package-update
  :ensure t
  :init (auto-package-update-maybe))

;; no-littering is useful to de-clutter my /.emacs.d directory
(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))

(use-package no-littering)
(require 'no-littering)

;; load up all literate org-mode files in user-emacs-directory
(mapc #'org-babel-load-file (directory-files user-emacs-directory t "\\.org$"))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-completing-read+

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    ;; flycheck
    flycheck
    flycheck-clj-kondo

    graphviz-dot-mode
    clj-refactor
    expand-region
    helm
    helm-projectile
    avy
    emojify
    company
    solarized-theme
    markdown-mode))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")

(defun switch-to-previous-buffer ()
  "Switch to most recent buffer. Repeated calls toggle back and forth between the most recent two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; set key binding
(global-set-key (kbd "C-`") 'switch-to-previous-buffer)

(electric-indent-mode +1)

;; (textmate-mode)

(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "C-x g") 'magit-status)

(with-eval-after-load 'magit
  (magit-change-popup-key 'magit-fetch-popup  :action ?u ?f)
  (magit-change-popup-key 'magit-pull-popup   :action ?u ?F)
  (magit-change-popup-key 'magit-rebase-popup :action ?e ?r)
  (magit-change-popup-key 'magit-push-popup   :action ?p ?P))

(global-set-key (kbd "s-r") 'ace-window)
(setq aw-dispatch-always 't)

(global-set-key (kbd "s-1") (kbd "s-r 1"))
(global-set-key (kbd "s-2") (kbd "s-r 2"))
(global-set-key (kbd "s-3") (kbd "s-r 3"))
(global-set-key (kbd "s-4") (kbd "s-r 4"))
(global-set-key (kbd "s-5") (kbd "s-r 5"))
(global-set-key (kbd "s-6") (kbd "s-r 6"))
(global-set-key (kbd "s-7") (kbd "s-r 7"))
(global-set-key (kbd "s-8") (kbd "s-r 8"))
(global-set-key (kbd "s-9") (kbd "s-r 9"))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "<s-return>") (kbd "C-e C-m"))
(global-set-key (kbd "<S-s-return>") (kbd "C-p C-e C-m"))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

(require 'helm-projectile)
(helm-projectile-on)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

(global-set-key (kbd "M-\\") 'shrink-whitespace)

(avy-setup-default)
(global-set-key (kbd "s-d") 'avy-goto-word-1)

(delete-selection-mode t)

(global-emojify-mode)
(setq emojify-inhibit-major-modes (list 'ruby-mode 'magit-mode))

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   '(fira-code-mode volatile-highlights use-package tagedit solarized-theme smex rainbow-delimiters parinfer org-plus-contrib no-littering markdown-mode magit ido-completing-read+ helm-projectile graphviz-dot-mode gnu-elpa-keyring-update flycheck-pos-tip flycheck-clj-kondo expand-region exec-path-from-shell evil-commentary evil-collection emojify delight company clojure-mode-extra-font-locking clj-refactor avy auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
