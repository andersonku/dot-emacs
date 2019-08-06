(setq user-full-name "Anderson Ku"
      user-mail-address "andersonku@gmail.com")

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(setq message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;;; 
(defconst emacs-start-time (current-time))

;;; My elisp director
(add-to-list 'load-path "~/.emacs.d/elisp")

;;----------------------------------------------------------------------------
;; Setup use-package
;;----------------------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;;----------------------------------------------------------------------------
;; Settings
;;----------------------------------------------------------------------------
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Save History
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Disable Toolbar
(tool-bar-mode -1)

(setq sentence-end-double-space nil)

(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;; Killing text
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

;;; Show column number
(column-number-mode 1)

;;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'\\|\\.json\\'" . js2-mode))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)))

;;----------------------------------------------------------------------------
;;; Packages
;;----------------------------------------------------------------------------

(use-package js2-mode
  :mode "\\.js\\'"
  :bind (:map js2-mode-map ("C-c C-c" . compile)))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;;; Autocomplete
(use-package company
  :config (add-hook 'prog-mode-hook 'company-mode))

;; Winner mode - undo and redo window configuration
;; lets you use C-c <left> and C-c <right>
(use-package winner
  :init
  (winner-mode))

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("<f1> h" . helm-mini)
         ("<f1> a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

;;; Display a more compact mode line
(use-package smart-mode-line)

;;; Minibuffer editing - more space!
(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

;;; This lets you use C-x u (undo-tree-visualize) to visually walk through the changes you've made, undo back to a certain point (or redo), and go down different branches.
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;;; Helm-swoop - quickly finding lines
;;; This promises to be a fast way to find things. Let's bind it to Ctrl-Shift-S to see if I can get used to that…
(use-package helm-swoop
 :bind
 (("C-S-s" . helm-swoop)
  ("M-i" . helm-swoop)
  ("M-s s" . helm-swoop)
  ("M-s M-s" . helm-swoop)
  ("M-I" . helm-swoop-back-to-last-point)
  ("C-c M-i" . helm-multi-swoop)
  ("C-x M-i" . helm-multi-swoop-all)
  )
 :config
 (progn
   (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
   (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
 )

;;; Recent files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

;;; Snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
  (progn
    (yas-global-mode)
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (setq yas-key-syntaxes '("w_" "w_." "^ "))
    (setq yas-installed-snippets-dir "~/elisp/yasnippet-snippets")
    (setq yas-expand-only-for-last-commands nil)
    (yas-global-mode 1)
    (bind-key "\t" 'hippie-expand yas-minor-mode-map)
    (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt)))
;;        (global-set-key (kbd "C-c y") (lambda () (interactive)
;;                                         (yas/load-directory "~/elisp/snippets")))

(use-package avy)


(use-package avy-zap
  :bind
  (("M-z" . avy-zap-up-to-char-dwim)
   ("M-Z" . avy-zap-to-char-dwim)))

(use-package rainbow-delimiters :disabled t)

;;----------------------------------------------------------------------------
;;; Theme
;;----------------------------------------------------------------------------
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))


;;----------------------------------------------------------------------------
;;; From my old config file
;;----------------------------------------------------------------------------
(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When `universal-argument' is called first, copy whole buffer (but respect `narrow-to-region')."
  (interactive)
  (let (p1 p2)
    (if (null current-prefix-arg)
        (progn (if (use-region-p)
                   (progn (setq p1 (region-beginning))
                          (setq p2 (region-end)))
                 (progn (setq p1 (line-beginning-position))
                        (setq p2 (line-end-position)))))
      (progn (setq p1 (point-min))
             (setq p2 (point-max))))
    (kill-ring-save p1 p2)))

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (but respect `narrow-to-region')."
  (interactive)
  (let (p1 p2)
    (if (null current-prefix-arg)
        (progn (if (use-region-p)
                   (progn (setq p1 (region-beginning))
                          (setq p2 (region-end)))
                 (progn (setq p1 (line-beginning-position))
                        (setq p2 (line-beginning-position 2)))))
      (progn (setq p1 (point-min))
             (setq p2 (point-max))))
    (kill-region p1 p2)))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "<f2>") 'xah-cut-line-or-region)  ; cut
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f10>")   'bm-next)
(global-set-key (kbd "<S-f10>") 'bm-previous)
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region) ; copy
(global-set-key [C-f3] 'highlight-symbol-at-point)
(global-set-key (kbd "<f4>") 'yank)
(global-set-key [(f5)] 'gud-cont)
(global-set-key [(f7)] 'recompile)
(global-set-key [C-f7] 'compile)
(global-set-key (kbd "<f8>") 'recentf-open-files)
(global-set-key (kbd "<f9>") 'ff-get-other-file)
(global-set-key [(f10)] 'gud-next)
(global-set-key [(f11)] 'gud-step)
(global-set-key [(f12)] (lambda () (interactive) (kill-buffer (current-buffer))))
;;; global-set-key [(f12)] 'delete-frame)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)


(define-key global-map (kbd "C-z") 'undo-tree-undo)
(define-key global-map (kbd "C-S-z") 'undo-tree-redo)
(define-key global-map (kbd "C-;") 'evilnc-comment-or-uncomment-lines)
(define-key global-map (kbd "C-'") 'rg)
(define-key global-map (kbd "C-x C-z") 'fzf)
(define-key global-map (kbd "C-`") 'fzf-projectile)

(define-key global-map (kbd "M-RET") 'ace-jump-mode)
(global-set-key [(control up)]  'move-line-up)
(global-set-key [(control down)]  'move-line-down)
(global-set-key [(control l)] 'kill-whole-line)
(global-set-key [?\e deletechar] 'kill-word)

(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

;;; My Key Bindings
(global-set-key [?\C-h] 'delete-backward-char)

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

;;; Finalization
(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f" default)))
 '(package-selected-packages
   (quote
    (company yasnippet helm-swoop miniedit diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;----------------------------------------------------------------------------
;; Goanna stuff
;;----------------------------------------------------------------------------

(load "~/.emacs.d/elisp/goanna/gisl-mode.el")
(load "~/.emacs.d/elisp/goanna/gpsl-mode.el")
(load "~/.emacs.d/elisp/goanna/gxsl-mode.el")

(setq initial-buffer-choice "~/.emacs.d/init.el")
