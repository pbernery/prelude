;;;;
;; PRELUDE CONFIGURATION
;;;;

;; Packages
(prelude-require-packages '(all-the-icons
                            all-the-icons-ivy
                            ember-mode
                            ibuffer-projectile
                            neotree
                            spacemacs-theme))

;; Set theme
;; (setq prelude-theme nil)
;; (load-theme 'spacemacs-light)


;;;;
;; PACKAGE CONFIGURATION
;;;;

;;;; ivy
(ivy-mode 1)

;;;; ibuffer
(ibuffer-projectile-set-filter-groups)

;;;; neotree
(setq neo-theme 'icons)
(setq neo-smart-open t)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;;;; ruby-mode
;; Disable insertion of coding header
(setq enh-ruby-add-encoding-comment-on-save nil)
(setq ruby-insert-encoding-magic-comment nil)

;;;; web-mode
(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

;;;;
;; Keys

;; Rebound s-/, because it is bound to hippie-expand by prelude-macos
(define-key prelude-mode-map (kbd "s-/") 'comment-or-uncomment-region-or-line)


;; Custom configuration

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(package-selected-packages
   (quote
    (spacemacs-themes zop-to-char zenburn-theme which-key volatile-highlights undo-tree smartrep smartparens smart-mode-line operate-on-number move-text magit imenu-anywhere ibuffer-projectile hl-todo guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region exec-path-from-shell ember-mode editorconfig easy-kill discover-my-major diminish diff-hl crux browse-kill-ring beacon anzu ace-window)))
 '(safe-local-variable-values (quote ((flycheck-disabled-checkers emacs-lisp-checkdoc)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
