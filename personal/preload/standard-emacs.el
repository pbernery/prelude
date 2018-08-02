;; Put temp files outside file folder
;; Fix for Ember Broccoli
;; https://ember-cli.com/user-guide/#emacs
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; Window size
(when window-system (set-frame-size (selected-frame) 104 58))

;; Frame appearance (works with emacs-plus)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Fonts
(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Cochin" :height 165))
  (buffer-face-mode))

(defun my-buffer-face-mode-fixed ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Hack" :height 120))
  (buffer-face-mode))

;; (add-hook 'text-mode-hook 'my-buffer-face-mode-variable)
(add-hook 'prog-mode-hook 'my-buffer-face-mode-fixed)

;; macOS specific
(setq-default mac-right-option-modifier nil)

;; Feature activation
(global-linum-mode 1)
