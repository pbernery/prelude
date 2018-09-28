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
                            robe
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

(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

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
 '(package-selected-packages
   (quote
    (robe avy company dash flycheck ghub git-commit helm iedit ivy lsp-mode magit markdown-mode multiple-cursors projectile pythonic typescript-mode yasnippet zop-to-char zenburn-theme yari yaml-mode which-key web-mode volatile-highlights undo-tree tide spacemacs-theme smartrep smartparens smart-mode-line ruby-refactor rainbow-mode rainbow-delimiters operate-on-number neotree move-text magit-todos lsp-ruby lsp-javascript-typescript less-css-mode json-mode js2-refactor inf-ruby imenu-anywhere ibuffer-projectile helm-projectile guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist geiser feature-mode expand-region exec-path-from-shell emr ember-mode elisp-slime-nav editorconfig edit-indirect easy-kill dockerfile-mode discover-my-major diminish diff-hl crux counsel company-tern company-lsp company-anaconda browse-kill-ring beacon apib-mode anzu anaphora all-the-icons-ivy ag ace-window a)))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (add-to-list
            (quote exec-path)
            (concat
             (locate-dominating-file default-directory ".dir-locals.el")
             "node_modules/.bin/")))
     (eval setq js-switch-indent-offset 0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
