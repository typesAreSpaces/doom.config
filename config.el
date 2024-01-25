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
;(setq doom-font (font-spec :family "Input Mono" :size 18 :weight 'semi-light)
;      doom-variable-pitch-font (font-spec :family "sans" :size 19))

(setq doom-font (font-spec :size 18)
      doom-big-font (font-spec :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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

; Path definitions

(setq phd-thesis-dir "~/Documents/GithubProjects/phd-thesis")
(setq phd-thesis-write-ups-dir
      (concat phd-thesis-dir
              "/Documents/Write-Ups"))
(setq maxdiff-write-ups-dir
      (concat phd-thesis-dir
              "/Documents/Side-Projects/MaxDiff/Documents/notes"))

; Custom keybindings

(defun haha ()
  (interactive)
  (message "haha"))

(map! :leader
      :desc "Something"
      "a" #'haha)

; Package configuration

(use-package! zoom
  :config
  (setq zoom-size '(0.618 . 0.618)))

(use-package! olivetti
  :custom
  (olivetti-body-width 130))

(use-package! yasnippet
  :config
  (setq yas-snippet-dirs `(,(expand-file-name "snippets" user-emacs-directory)))
  (setq yas-key-syntaxes '(yas-longest-key-from-whitespace "w_.()" "w_." "w_" "w"))
  (define-key yas-minor-mode-map (kbd "C-g") 'evil-normal-state)
  (define-key yas-keymap (kbd "C-g") 'evil-normal-state)
  (yas-global-mode 1))

(use-package! citar
  :custom
  (citar-bibliography `(,(concat maxdiff-write-ups-dir "/references.bib")
                        ,(concat phd-thesis-write-ups-dir "/references.bib"))))

(map! "C-c b" #'citar-insert-citation)

(use-package! jinx
  :config
  (vertico-multiform-mode 1))

(defun efs/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode))

(use-package! lsp-mode
      :commands (lsp lsp-deferred)
      ;:hook (lsp-mode . efs/lsp-mode-setup)
      :init
      (setq lsp-keymap-prefix "C-l")
      :config
      (setq lsp-completion-provider :none)
      (defun corfu-lsp-setup ()
        (setq-local completion-styles '(orderless)
                    completion-category-defaults nil))
      (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
      (lsp-enable-which-key-integration t))

(add-hook 'TeX-mode-hook 'lsp)
(add-hook 'LaTeX-mode-hook 'lsp)

(use-package! lsp-latex
  :config
  (setq lsp-latex-build-executable "latexmk")
  (setq lsp-latex-build-args
        '("-pvc" "-pdf" "-interaction=nonstopmode" "-synctex=1" "-cd" "%f"))
  (setq lsp-latex-forward-search-after t)
  (setq lsp-latex-build-on-save t)
  (setq lsp-latex-forward-search-executable "/opt/homebrew/bin/sioyek")
  (setq lsp-latex-forward-search-args
        '(
          "--forward-search-file"
          "%f"
          "--forward-search-line"
          "%l"
          "%p")))

(map! :map LaTeX-mode-map
      :prefix "C-l w"
      "b" #'lsp-latex-build)

(map! :leader
      :map LaTeX-mode-map
      "lf" #'lsp-latex-forward-search)

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
