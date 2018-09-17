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

;; macOS specific
(setq-default mac-right-option-modifier nil)

;; Feature activation
(global-linum-mode 1)

;; Set the fonts for Emojis
(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
(set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)
