;;;;
;; PRELUDE CONFIGURATION
;;;;

;; Packages
(prelude-require-packages '(ag
                            all-the-icons
                            all-the-icons-ivy
                            apib-mode
                            edit-indirect
                            ember-mode
                            emr
                            ibuffer-projectile
                            js2-refactor
                            magit-todos
                            neotree
                            ruby-refactor
                            spacemacs-theme
                            tide))

;; Disable Guru mode
(setq prelude-guru nil)

;; Set theme
;; (setq prelude-theme nil)
;; (load-theme 'spacemacs-light)


;;;;
;; PACKAGE CONFIGURATION
;;;;

;;; all-the-icons-ivy
(all-the-icons-ivy-setup)

;;; apib-mode
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

;;; emacs-refactor
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)

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
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)


;;;; magit
(magit-todos-mode)

;;;; markdown
(setq markdown-command "/usr/local/bin/multimarkdown")

;;;; multiple-cursor
(multiple-cursors-mode 1)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

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

;; ruby-refactor
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

;;;; tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'js2-mode-hook #'setup-tide-mode)

;; configure javascript-tide checker to run after your default javascript checker
(with-eval-after-load 'tide-mode
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

;;;; web-mode
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

;;;;
;; Keys

;; Rebound s-/, because it is bound to hippie-expand by prelude-macos
(define-key prelude-mode-map (kbd "s-/") 'comment-or-uncomment-region-or-line)

(global-set-key (kbd "M-t") 'neotree-project-dir)
(global-set-key (kbd "M-[")
                (lambda (b e n)
                  (interactive "r\nP")
                  (indent-rigidly-left-to-tab-stop b e)
                  (setq deactivate-mark nil)))

(global-set-key (kbd "M-]")
                (lambda (b e n)
                  (interactive "r\nP")
                  (indent-rigidly-right-to-tab-stop b e)
                  (setq deactivate-mark nil)))

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
    (apib-mode all-the-icons-ivy helm-core helm-projectile spacemacs-theme edit-indirect a anaconda-mode avy company ghub git-commit projectile pythonic web-mode ruby-refactor emr ag counsel helm ivy swiper dockerfile-mode js2-refactor company-tern magit-todos markdown-mode spacemacs-themes zop-to-char zenburn-theme which-key volatile-highlights undo-tree smartrep smartparens smart-mode-line operate-on-number move-text magit imenu-anywhere ibuffer-projectile hl-todo guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region exec-path-from-shell ember-mode editorconfig easy-kill discover-my-major diminish diff-hl crux browse-kill-ring beacon anzu ace-window)))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (add-to-list
            (quote exec-path)
            (concat
             (locate-dominating-file default-directory ".dir-locals.el")
             "node_modules/.bin/")))
     (eval setq js-switch-indent-offset 0)
     (flycheck-disabled-checkers emacs-lisp-checkdoc)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
