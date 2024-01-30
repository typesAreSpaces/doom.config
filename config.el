;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jose Abel Castellanos Joo"
      user-mail-address "jabelcastellanosjoo@unm.edu")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;;(setq doom-font (font-spec :family "Input Mono" :size 18 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "sans" :size 19))

(setq doom-font (font-spec :size 16)
      doom-big-font (font-spec :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-ellipsis "⇓")
(setq org-hierarchical-todo-statistics nil)
(setq org-directory "~/org/")
(setq org-agenda-files
      '(
        "~/Documents/GithubProjects/phd-thesis/Documents/Misc/20231115200616-qm_seminar.org"
        "~/Documents/GithubProjects/phd-thesis/Documents/Org-Files/research_tasks.org"
        "~/Documents/GithubProjects/phd-thesis/Documents/Org-Files/school_tasks.org"
        "~/Documents/GithubProjects/phd-thesis/Documents/Org-Files/graduation_logistics.org"
        "~/Documents/GithubProjects/phd-thesis/Documents/Org-Files/dissertation_tasks.org"
        "~/Documents/GithubProjects/phd-thesis/Documents/Org-Files/main.org"
        ))
(setq org-file-apps
      '((auto-mode . emacs)
        (directory . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . "sioyek %s")
        ("\\.nb?\\'" . "Mathematica %s")))
(after! org
  (setq org-todo-keywords
        '((sequence "EXTERNAL" "|")
          (sequence "GOAL" "IDEA" "OBSERVATION" "|" "OK")
          (sequence "TODAY" "TODO" "LATER" "|" "MOVED" "COMPLETED(c)" "CANC(k@)")
          (sequence "EMAIL" "|"))))
                                        ;(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
(defun org-sort-buffer ()
  "Sort all entries in the current buffer, recursively."
  (interactive)
  (mark-whole-buffer)
  (org-sort-entries nil ?o)
  (org-map-entries (lambda ()
                     (condition-case nil
                         (org-sort-entries nil ?o)
                       (user-error)))))

(map! :map org-mode-map 
      "C-c d" #'(lambda () (interactive) (org-todo "MOVED")))
(map! :map org-mode-map 
      "C-c c" #'(lambda () (interactive) (org-todo "COMPLETED")))
(map! :map org-mode-map 
      "C-c t" #'(lambda () (interactive) (org-todo "TODO")))
(map! :map org-mode-map 
      "C-c k" #'(lambda () (interactive) (org-todo "CANC")))
(map! :map org-mode-map 
      "C-c i" #'(lambda () (interactive) (org-todo "IDEA")))
(map! :map org-mode-map 
      "C-c o" #'(lambda () (interactive) (org-todo "OK")))
(map! :map org-mode-map 
      "C-c C-<return>" #'org-insert-heading-respect-content)
(map! :map org-mode-map 
      "C-c C-<SPC>" #'org-insert-subheading)
(map! :map org-mode-map 
      "C-c RET" #'org-meta-return)
(map! :map org-mode-map 
      "C-c s" #'(lambda () (interactive) (org-sort-buffer)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Basic UI
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq make-backup-files nil)

;; Path definitions

(setq phd-thesis-dir "~/Documents/GithubProjects/phd-thesis")
(setq phd-thesis-org-files-dir
      (concat phd-thesis-dir
              "/Documents/Org-Files"))
(setq phd-thesis-write-ups-dir
      (concat phd-thesis-dir
              "/Documents/Write-Ups"))
(setq maxdiff-write-ups-dir
      (concat phd-thesis-dir
              "/Documents/Side-Projects/MaxDiff/Documents/notes"))
(setq scc-dir
      (concat phd-thesis-dir
              "/Documents/Side-Projects/kapur-nsf-proposal/2022"))
(setq scc-reports-dir (concat scc-dir "/Reports"))

;; Custom keybindings

(defun haha ()
  (interactive)
  (message "haha"))

(map! :leader
      :desc "Something"
      "z" #'haha)

(map! :leader
      :prefix "s"
      "c" #'avy-goto-char
      "w" #'avy-goto-word)

(map! :leader
      :prefix "r"
      "s" #'bookmark-set
      "j" #'consult-bookmark
      "d" #'bookmark-delete)

(map! :leader
      :prefix "b"
      "c" #'evilnc-comment-or-uncomment-lines
      "f" #'fill-paragraph
      "i" #'((lambda () (interactive)
               (indent-region (point-min) (point-max)))))

(map! :leader
      :prefix "w"
      "u" #'winner-undo
      "r" #'winner-redo)

(map! :leader
      :prefix "t"
      "n" #'tab-bar-new-tab
      "c" #'tab-bar-close-tab
      "d" #'tab-bar-duplicate-tab
      "r" #'tab-bar-renma-tab)

(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-c C-b") 'consult-buffer)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-c C-f") 'consult-find)
(global-set-key (kbd "C-c C-g") 'consult-grep)

(map! :after evil
      :map evil-normal-state-map
      "C-t" 'tab-bar-new-tab)
(map! :after evil
      :map evil-normal-state-map
      "C-<tab>" 'tab-bar-switch-to-next-tab)

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Package configuration

(use-package! zoom
  :config
  (setq zoom-size '(0.618 . 0.618)))

(use-package! olivetti
  :custom
  (olivetti-body-width 130))

(use-package! yasnippet
  :config
  (setq yas-snippet-dirs `(,(expand-file-name "snippets" doom-user-dir)))
  (setq yas-key-syntaxes '(yas-longest-key-from-whitespace "w_.()" "w_." "w_" "w"))
  (define-key yas-minor-mode-map (kbd "C-g") 'evil-normal-state)
  (define-key yas-keymap (kbd "C-g") 'evil-normal-state)
  (yas-global-mode 1))

(use-package! yasnippet-snippets)

(load! (expand-file-name "snippets/yasnippet-scripts.el" doom-user-dir))

(defun restart-yasnippet ()
  (interactive)
  (add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets))

(use-package! citar
  :custom
  (citar-bibliography `(,(concat scc-reports-dir "/references.bib")
                        ,(concat maxdiff-write-ups-dir "/references.bib")
                        ,(concat phd-thesis-write-ups-dir "/references.bib"))))

(map! "C-c b" #'citar-insert-citation)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package! lsp-mode
  :commands (lsp lsp-deferred)
  ;;:hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  ;;(setq lsp-completion-provider :company)
  (setq lsp-completion-provider :none)
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(orderless)
                completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
  (lsp-enable-which-key-integration t))

(add-hook 'TeX-mode-hook 'outline-minor-mode)
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
(add-hook 'TeX-mode-hook 'lsp)
(add-hook 'LaTeX-mode-hook 'lsp)
(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'TeX-mode-hook #'auto-fill-mode)
(add-hook 'LaTeX-mode-hook #'auto-fill-mode)
(setq-default fill-column 80)
(add-hook 'TeX-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'LaTeX-mode-hook #'display-fill-column-indicator-mode)

(use-package! lsp-latex
  :config
  (setq lsp-latex-build-executable "latexmk")
  (setq lsp-latex-build-args
        '("-pvc" "-pdf" "-interaction=nonstopmode" "-synctex=1" "-cd" "%f"))
  (setq lsp-latex-forward-search-after t)
  (setq lsp-latex-build-on-save t)
  ;; Configuration for sioyek
  ;; (setq lsp-latex-forward-search-executable "/opt/homebrew/bin/sioyek")
  ;; (setq lsp-latex-forward-search-args
  ;;       '(
  ;;         "--forward-search-file"
  ;;         "%f"
  ;;         "--forward-search-line"
  ;;         "%l"
  ;;         "%p"))
  ;; Configuration for Skim
  (setq lsp-latex-forward-search-executable
        "/Applications/Skim.app/Contents/SharedSupport/displayline")
  (setq lsp-latex-forward-search-args '("%l" "%p" "%f"))
  )

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-insert-label-flags (list t nil))
(setq reftex-ref-macro-prompt nil)
(setq font-latex-fontify-script nil)
(setq TeX-electric-sub-and-superscript nil)

(map! :map LaTeX-mode-map
      :prefix "C-l w"
      "b" #'lsp-latex-build)

(map! :leader
      :map LaTeX-mode-map
      "lf" #'lsp-latex-forward-search)
(map! :leader
      :map LaTeX-mode-map
      "lF" #'(lambda () (interactive) (LaTeX-fill-buffer nil)))
(map! :leader
      :map outline-minor-mode-map
      "<tab>" #'outline-toggle-children)
(map! :map outline-minor-mode-map
      "<S-tab>" #'outline-cycle)

(defun get-bibtex-from-doi (doi)
  "Get a BibTeX entry from the DOI"
  (interactive "MDOI: ")
  (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
    (with-current-buffer
        (url-retrieve-synchronously
         (format "http://dx.doi.org/%s"
                 (replace-regexp-in-string "http://dx.doi.org/" "" doi)))
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (setq bibtex-entry
            (buffer-substring
             (string-match "@" (buffer-string))
             (point)))
      (kill-buffer (current-buffer))))
  (insert (decode-coding-string bibtex-entry 'utf-8))
  (bibtex-fill-entry))

(use-package! atomic-chrome
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . poly-markdown+r-mode)
          ("overleaf\\.com" . latex-mode))))

(defhydra hydra-jump-files (:exit t)
  "jump to files"
  ("a" (find-file
        (expand-file-name (concat phd-thesis-org-files-dir "/main.org")))
   "Agenda")
  ("e" (find-file
        (expand-file-name "config.el" doom-user-dir))
   "Doom Emacs config")
  ("rp" (find-file
         (expand-file-name (concat phd-thesis-write-ups-dir "/references.bib")))
   "Bibtex references - PhD thesis")
  ("rs" (find-file
         (expand-file-name (concat scc-reports-dir "/references.bib")))
   "Bibtex references - SCC project"))

(map! :leader
      "C-f" #'hydra-jump-files/body)

(use-package! cape
                                        ; Bind dedicated completion commands
                                        ; Alternative prefix keys: C-c p, M-p, M-+, ...
                                        ;:bind (("C-c p p" . completion-at-point) ; capf
                                        ;       ("C-c p t" . complete-tag)        ; etags
                                        ;       ("C-c p d" . cape-dabbrev)        ; or
                                        ; dabbrev-completion
                                        ;       ("C-c p h" . cape-history)
                                        ; ("C-c p f" . cape-file)
                                        ;       ("C-c p k" . cape-keyword)
                                        ;       ("C-c p s" . cape-elisp-symbol)
                                        ;       ("C-c p e" . cape-elisp-block)
                                        ; ("C-c p a" . cape-abbrev)
                                        ;       ("C-c p l" . cape-line)
                                        ;       ("C-c p w" . cape-dict)
                                        ;       ("C-c p :" . cape-emoji)
                                        ;       ("C-c p \\" . cape-tex)
                                        ;       ("C-c p _" . cape-tex)
                                        ;       ("C-c p ^" . cape-tex)
                                        ;       ("C-c p &" . cape-sgml)
                                        ;       ("C-c p r" . cape-rfc1345))
  :init
                                        ; Add to the global default value of
                                        ; `completion-at-point-functions' which is
                                        ; used by `completion-at-point'.  The order of the functions
                                        ; matters, the
                                        ; first function returning a result wins.  Note that the list
                                        ; of buffer-local
                                        ; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
                                        ; (add-to-list 'completion-at-point-functions
                                        ; #'cape-elisp-block)
                                        ; (add-to-list 'completion-at-point-functions #'cape-history)
                                        ; (add-to-list 'completion-at-point-functions #'cape-keyword)
                                        ; (add-to-list 'completion-at-point-functions #'cape-tex)
                                        ; (add-to-list 'completion-at-point-functions #'cape-sgml)
                                        ; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
                                        ; (add-to-list 'completion-at-point-functions #'cape-abbrev)
                                        ; (add-to-list 'completion-at-point-functions #'cape-dict)
                                        ; (add-to-list 'completion-at-point-functions
                                        ; #'cape-elisp-symbol)
                                        ; (add-to-list 'completion-at-point-functions #'cape-line))
  )
