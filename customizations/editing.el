;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)


;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (setq-default evil-auto-indent t
                evil-cross-lines t
                evil-default-cursor t
                evil-default-state 'normal
                evil-echo-state nil
                evil-ex-search-case 'smart
                evil-ex-search-vim-style-regexp t
                evil-magic 'very-magic
                evil-search-module 'evil-search
                evil-shift-width 2)

  ;; Avoid dropping into insert mode in compilation windows
  (add-hook 'compilation-start-hook 'evil-normal-state)

  (defun ef-kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

  (define-key evil-normal-state-map ",kob" 'ef-kill-other-buffers)
  (define-key evil-normal-state-map ",kb" 'kill-this-buffer)

  (defun ef-kill-buffer-or-delete-window ()
    "If more than one window is open, delete the current window, otherwise kill current buffer"
    (interactive)
    (if (> (length (window-list)) 1)
        (delete-window)
      (kill-buffer)))

  (evil-ex-define-cmd "q" 'ef-kill-buffer-or-delete-window)

  (defun ef-indent-buffer ()
    "Indent the currently visited buffer."
    (interactive)
    (indent-region (point-min) (point-max)))

  (define-key evil-normal-state-map (kbd ", TAB") 'ef-indent-buffer)

  (define-key evil-visual-state-map (kbd "TAB") 'indent-region)
  (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)

  (define-key evil-normal-state-map ",i" 'imenu)
  (define-key evil-normal-state-map ",ws" 'delete-trailing-whitespace)

  ;; Alignment
  (defun ef-align-to-= (begin end)
    "Align region to = signs"
    (interactive "r")
    (align-regexp begin end "\\(\\s-*\\)=" 1 1))

  (evil-define-key 'visual prog-mode-map ",=" 'ef-align-to-=)

  ;; Easier window navigation
  (define-key evil-normal-state-map (kbd "s-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "s-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "s-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "s-l") 'evil-window-right)

  ;; Text-scaling
  (define-key evil-normal-state-map ",-" 'text-scale-adjust)
  (define-key evil-normal-state-map ",+" 'text-scale-adjust)
  (define-key evil-normal-state-map ",=" 'text-scale-adjust)

  ;; Comint history
  (evil-define-key 'insert comint-mode-map (kbd "<up>") 'comint-previous-input)
  (evil-define-key 'insert comint-mode-map (kbd "<down>") 'comint-next-input)

  ;; Unset some annoying keys
  (define-key evil-motion-state-map "K" nil)
  (define-key evil-normal-state-map "K" nil)

  (defvar ef-toggle-scratch--prev-buffer nil)

  (defun ef-toggle-scratch--goto-scratch ()
    (if-let* ((scratch-buffer (get-buffer "*scratch*")))
        (progn
          (setq ef-toggle-scratch--prev-buffer (current-buffer))
          (switch-to-buffer scratch-buffer))
      (message "No *scratch* buffer found.")))

  (defun ef-toggle-scratch--goto-prev-buffer ()
    (if (buffer-live-p ef-toggle-scratch--prev-buffer)
        (switch-to-buffer ef-toggle-scratch--prev-buffer)
      (message "No buffer to switch back to.")))

  (defun ef-toggle-scratch ()
    "Toggle between *scratch* buffer and the current buffer."
    (interactive)
    (if (equal (buffer-name) "*scratch*")
        (ef-toggle-scratch--goto-prev-buffer)
      (ef-toggle-scratch--goto-scratch)))

  (define-key evil-normal-state-map ",S" 'ef-toggle-scratch)

  (evil-add-hjkl-bindings package-menu-mode-map 'emacs
                          "H" 'package-menu-quick-help))

(use-package undo-tree
  :after evil
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-lazy-drawing nil)
  (setq undo-tree-auto-save-history t)
  (let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
    (setq undo-tree-history-directory-alist (list (cons "." undo-dir))))

  (define-key evil-normal-state-map (kbd ",u") 'undo-tree-visualize))

(use-package whitespace
  :init
  (defvar whitespace-cleanup-before-save t)
  (defun my-whitespace-cleanup-before-save ()
    (if whitespace-cleanup-before-save
      (whitespace-cleanup)))
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook 'my-whitespace-cleanup-before-save)
  ;; Except Markdown
  (add-hook 'markdown-mode-hook
    '(lambda ()
       (set (make-local-variable 'whitespace-cleanup-before-save) nil)))
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; https://github.com/DogLooksGood/parinfer-mode
(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             evil           ; If you use Evil.
             ;;lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

(defun whack-whitespace-forward ()
  "Delete all white space from point to the next word."
  (interactive nil)
  (re-search-forward "[ \t\n]+" nil t)
  (replace-match "" nil nil))

(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of a line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

(global-set-key (kbd "C-M-+") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)
(global-set-key (kbd "M-[") 'whack-whitespace-forward)
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
