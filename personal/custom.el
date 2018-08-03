;;;;
;; PRELUDE CONFIGURATION
;;;;

;; Packages
(prelude-require-packages '(ag
                            all-the-icons
                            all-the-icons-ivy
                            company-tern
                            ember-mode
                            ibuffer-projectile
                            js2-refactor
                            magit-todos
                            neotree
                            spacemacs-theme))

;; Disable Guru mode
(setq prelude-guru nil)

;; Set theme
;; (setq prelude-theme nil)
;; (load-theme 'spacemacs-light)


;;;;
;; PACKAGE CONFIGURATION
;;;;

;;;; hs-minor-mode
(global-unset-key (kbd "s-f"))
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'hs-minor-mode-hook
          (lambda ()
            (define-key hs-minor-mode-map (kbd "s-f") 'hs-toggle-hiding)))

;;;; ivy
(ivy-mode 1)

;;;; ibuffer
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)))

;;;; js2-mode

;; refactor
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; completion
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

;;;; magit
;; (magit-todos-mode)

;;;; markdown
(setq markdown-command "/usr/local/bin/multimarkdown")

;;;; neotree
(setq neo-theme 'icons)
(setq neo-smart-open t)
(setq neo-window-width 40)

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

;; Switch project in Neotree
;; https://github.com/syl20bnr/spacemacs/issues/5682
(setq projectile-switch-project-action 'neotree-projectile-action)


;;;; org-mode
;; fontify code in code blocks
(setq org-src-fontify-natively t)

;;;; ruby-mode
;; Disable insertion of coding header
(setq enh-ruby-add-encoding-comment-on-save nil)
(setq ruby-insert-encoding-magic-comment nil)

;;;; web-mode
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

;;;;
;; Keys

;; Rebound s-/, because it is bound to hippie-expand by prelude-macos
(define-key prelude-mode-map (kbd "s-/") 'comment-or-uncomment-region-or-line)

(global-set-key (kbd "M-t") 'neotree-project-dir)

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
    (ag counsel helm ivy swiper dockerfile-mode js2-refactor company-tern magit-todos markdown-mode spacemacs-themes zop-to-char zenburn-theme which-key volatile-highlights undo-tree smartrep smartparens smart-mode-line operate-on-number move-text magit imenu-anywhere ibuffer-projectile hl-todo guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region exec-path-from-shell ember-mode editorconfig easy-kill discover-my-major diminish diff-hl crux browse-kill-ring beacon anzu ace-window)))
 '(safe-local-variable-values
   (quote
    ((eval setq js-switch-indent-offset 0)
     (flycheck-disabled-checkers emacs-lisp-checkdoc)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
