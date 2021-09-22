;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; -----
(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

;; ---------------------------------------------------------
;;  My Setting
;;   after edit, re byte-compile
;;   $ emacs --batch -f batch-byte-compile init.el

;; (leaf leaf-convert
;;   :setq ((my-system-type . "wsl")) ; linux or darwin
;; )

(leaf my-settings
  :load-path "~/.emacs.d/site-lisp"
  :custom
  ((make-backup-files . nil)
   (indent-tabs-mode . nil)
   (tab-width . 4)
   (line-spacing . 0) ; row-space
  )
  :config
  (set-language-environment "UTF-8")
  (electric-indent-mode -1) ; invert C-j and Return key
  (show-paren-mode t) ; hi-light bracket((), {}...)
  (icomplete-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
)

(leaf my-key-bindings
  :config
  (global-set-key "\C-h" 'delete-backward-char)
  (global-set-key "\M-g" 'goto-line)
  (global-set-key "\C-x\C-b" 'buffer-menu)
  (global-set-key (kbd "<f12>") 'eshell)
  (global-unset-key "\C-z")
)

(leaf my-disp-settings
  :custom
  (inhibit-startup-screen . t)
  :config
  (line-number-mode t)
  (column-number-mode t)
  (setcar mode-line-position
          '(:eval (format "%d"
                          (count-lines
                           (point-max)
                           (point-min)))))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (set-scroll-bar-mode nil)
  (setq frame-title-format (format "%%f - Emacs@%s" (system-name)))
)

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1" "cl-lib-0.6"
  :tag "environment" "unix" "emacs>=24.1"
  :url "https://github.com/purcell/exec-path-from-shell"
  :added "2021-09-15"
  :emacs>= 24.1
  :ensure t
  :custom
  ((exec-path-from-shell-arguments . '("-l")))
  :config
  (exec-path-from-shell-initialize)
)

(leaf windmove
  :doc "directional window-selection routines"
  :tag "builtin"
  :added "2021-09-15"
  :custom
  ((windmove-wrap-around . t))
  :config
  (windmove-default-keybindings)
)

(leaf cua-mode
  :init
  (cua-mode t)
  :custom
  ((cua-enable-cua-keys . nil))
  :config
  (define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)
)

(leaf uniquify
  :doc "unique buffer names dependent on file name"
  :tag "builtin" "files"
  :added "2021-09-15"
  :require t
  :custom
  ((uniquify-buffer-name-style . 'post-forward-angle-brackets)
   (uniquify-ignore-buffers-re .  "*[^*]+*") ;; correspond to change the buffername.
   (uniquify-min-dir-content . 1) ;; display if the same file is not opned.
  )
)

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :req "emacs-27.1"
  :tag "emacs>=27.1"
  :url "https://github.com/minad/vertico"
  :added "2021-09-15"
  :emacs>= 27.1
  :ensure t
  :custom
  ((vertico-count . 20))
)

(leaf consult
  :doc "Consulting completing-read"
  :req "emacs-26.1"
  :tag "emacs>=26.1"
  :url "https://github.com/minad/consult"
  :added "2021-09-15"
  :emacs>= 26.1
  :ensure t
  :after t embark
  :require embark-consult
)

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :req "emacs-26.1"
  :tag "emacs>=26.1"
  :url "https://github.com/minad/marginalia"
  :added "2021-09-15"
  :emacs>= 26.1
  :ensure t)

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :req "emacs-26.1"
  :tag "extensions" "emacs>=26.1"
  :url "https://github.com/oantolin/orderless"
  :added "2021-09-15"
  :emacs>= 26.1
  :ensure t
  ;; :config
  ;; (with-eval-after-load 'orderless
  ;;   (setq completion-styles '(orderless)))
  :after t
  :setq ((completion-styles quote
                            (orderless)))
)

(leaf enable-vertico-and-marginalia
  :preface
  (defun after-init-hook ()
    (vertico-mode)
    (marginalia-mode)
    (savehist-mode))
  :config
  (add-hook 'after-init-hook #'after-init-hook)
)

(leaf embark
  :doc "Conveniently act on minibuffer completions"
  :req "emacs-26.1"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/oantolin/embark"
  :added "2021-09-15"
  :emacs>= 26.1
  :ensure t)

(leaf elscreen
  :doc "Emacs window session manager"
  :req "emacs-24"
  :tag "convenience" "window" "emacs>=24"
  :url "https://github.com/knu/elscreen"
  :added "2021-09-15"
  :emacs>= 24
  :ensure t
  :init
  (elscreen-start)
  :custom
  ((elscreen-display-tab . nil))
)

(leaf wgrep
  :doc "Writable grep buffer and apply the changes to files"
  :tag "extensions" "edit" "grep"
  :url "http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el"
  :added "2021-09-15"
  :ensure t
  :init
  (require 'wgrep nil t)
  :custom
  ((wgrep-auto-save-buffer . t)
   (wgrep-change-readonly-file . t)
   (wgrep-enable-key . "e"))
)

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "dash-20210330" "git-commit-20210806" "magit-section-20210806" "transient-20210701" "with-editor-20210524"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :url "https://github.com/magit/magit"
  :added "2021-09-15"
  :emacs>= 25.1
  :ensure t
  :require t
  :after git-commit magit-section with-editor
)

(leaf open-junk-file
  :doc "Open a junk (memo) file to try-and-error"
  :tag "tools" "convenience"
  :url "http://www.emacswiki.org/cgi-bin/wiki/download/open-junk-file.el"
  :added "2021-09-15"
  :ensure t
  :require t
  :config
  (if (eq system-type 'gnu/linux)
      (setq junk-dir-root "~/win_home/notes/junk")
      (setq junk-dir-root "~/works/notes/junk"))
  (setq open-junk-file-format (concat junk-dir-root "/%y%m%d-%H%M%S."))
  (global-set-key "\C-xj" 'open-junk-file)
)

(leaf tramp
  :doc "Transparent Remote Access, Multiple Protocol"
  :tag "builtin"
  :added "2021-09-15"
  :require t
  :custom
  ((tramp-default-method . "ssh"))
)

(leaf insert-date-time
  :preface
  (defun my-insert-date nil
    (interactive)
    (setq system-time-locale "C")
    (insert
     (concat
      (format-time-string "%Y-%m-%d %a"))))

  (defun my-insert-time nil
    (interactive)
    (setq system-time-locale "C")
    (insert
     (concat
      (format-time-string "%H:%M:%S"))))
  :config
  (global-set-key [(control ?\;)] 'my-insert-date)
  (global-set-key [(control ?\:)] 'my-insert-time)
)

;; (leaf auto-complete
;;   :doc "Auto Completion for GNU Emacs"
;;   :req "popup-0.5.0" "cl-lib-0.5"
;;   :tag "convenience" "completion"
;;   :url "https://github.com/auto-complete/auto-complete"
;;   :added "2021-09-15"
;;   :ensure t
;;   :require t
;;   :config
;;   (global-auto-complete-mode t)
;; )

;; (leaf auto-complete-config
;;   :require t
;;   :after auto-complete
;;   :config
;;   (add-to-list 'ac-modes 'text-mode)         ;; enable text-mode by auto
;;   (add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
;;   (add-to-list 'ac-modes 'org-mode)
;;   (ac-set-trigger-key "TAB")
;;   :custom
;;   ((ac-use-menu-map . t)                   ;; display candidate on menu by C-n/C-p
;;    (ac-use-fuzzy . t))                      ;; match vague
;; )

(leaf company
  :ensure t
  :leaf-defer nil
  :blackout company-mode
  :bind
  ((company-active-map
    ("M-n" . nil)
    ("M-p" . nil)
    ("C-s" . company-filter-candidates)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-i" . company-complete-selection))
   (company-search-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)))
  :custom
  ((company-tooltip-limit         . 12)
   (company-idle-delay            . 0) ;; 補完の遅延なし
   (company-minimum-prefix-length . 1) ;; 1文字から補完開始
   (company-transformers          . '(company-sort-by-occurrence))
   (global-company-mode           . t)
   (company-selection-wrap-around . t)
   (company-require-match         . 'never))
  ;; :config
  ;; (global-set-key (kbd "TAB") 'company-complete-common-or-cycle)
  ;; (push 'company-preview-common-frontend company-frontends)
)

(leaf dired
  :init
  (load "dired-x")
  :config
  (defvar my-dired-before-buffer nil)
  (defadvice dired-advertised-find-file
    (before kill-dired-buffer activate)
    (setq my-dired-before-buffer (current-buffer)))
  (defadvice dired-advertised-find-file
    (after kill-dired-buffer-after activate)
    (if (eq major-mode 'dired-mode)
        (kill-buffer my-dired-before-buffer)))
  (defadvice dired-up-directory
    (before kill-up-dired-buffer activate)
    (setq my-dired-before-buffer (current-buffer)))
  (defadvice dired-up-directory
    (after kill-up-dired-buffer-after activate)
    (if (eq major-mode 'dired-mode)
        (kill-buffer my-dired-before-buffer)))
  ;; Customize Date/Time format
  ;;   -L : シンボリックリンクをディレクトリとして表示
  (setq dired-listing-switches
        "-alh --group-directories-first --time-style \"+%y-%m-%d %H:%M:%S\"")
  ;; 表示項目を指定
  (require 'dired-details-s)
  (setq dired-details-s-types
        '((size-time  . (user group size time))
          (all        . (perms links user group size time))
          (no-details . ())))
)

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :url "http://github.com/joaotavora/yasnippet"
  :added "2021-09-15"
  :ensure t
  :require t
  :custom
  ((yas-snippet-dirs . '("~/.emacs.d/mysnippets"
                         "~/.emacs.d/yasnippets")))
  :config
  ;; insert snippet
  (define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
  ;; create new snippet
  (define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
  ;; view/edit snippet file
  (define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
  (yas-global-mode 1)
)

(leaf org
  :doc "Export Framework for Org Mode"
  :tag "builtin"
  :added "2021-09-15"
  :custom
  (;; 見出しの余分な*を消す
   (org-hide-leading-stars . t)
   ;; 画像をインラインで表示
   (org-startup-with-inline-images . t)
   (org-directory . "~/win_home/notes")
   (org-directory . "notes.org")
   ;; org-directory内のファイルすべてからagendaを作成する
   (my-org-agenda-dir . "~/win_home/notes")
   (org-agenda-files . (list my-org-agenda-dir))
   ;; TODO
   (org-todo-keywords .  '((sequence "TODO(t)"
                                     "WORK(w)"
                                     "WAIT"
                                     "|"
                                     "DONE(d)"
                                     "CANCELED(c)" )))
   ;; DONEの時刻を記録
   (org-log-done . 'time)
   (org-capture-templates .
                          '(("l"
                             "Log / 作業ログ"
                             entry (file+datetree "~/win_home/notes/logs.org")
                             "* %?\n  Entered on %U"
                             :unnarrowed 1)
                            ("t"
                             "Task"
                             entry (file+datetree "~/win_home/notes/logs.org")
                             "** TODO %?\n   SCHEDULED: %^t")
                            ("v"
                             "View"
                             plain (file+datetree "~/win_home/notes/logs.org")
                             nil
                             :jump-to-captured 1
                             :unnarrowed 1)
                            ))
   )
  :config
   ;; .orgファイルは自動的にorg-mode
   (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
   (global-set-key "\C-cl" 'org-store-link)
   (global-set-key "\C-cc" 'org-capture)
   (global-set-key "\C-ca" 'org-agenda)
   (global-set-key "\C-cb" 'org-iswitchb)
   ;; color
   (custom-set-faces
    '(org-level-1 ((t (:foreground "#3498DB")))) ; PETER RIVER
    '(org-level-2 ((t (:foreground "#27AE60")))) ; NEPHRITIS
    '(org-level-3 ((t (:foreground "#2ECC71")))) ; EMERALD
    '(org-level-4 ((t (:foreground "#F39C12")))) ; GREEN SEA
    ;; org-level-3,4,5..と指定可能
    )
)

(leaf markdown
  :config
  (leaf markdown-mode
    :doc "Major mode for Markdown-formatted text"
    ;; ------------------------------
    ;;   key-bind
    ;;     TAB:          見出しやツリーの折り畳み
    ;;     C-c C-n:      次の見出しに移動
    ;;     C-c C-p:      前の見出しに移動
    ;;     C-c ← →:    見出しレベルの上げ下げ
    ;;     C-c ↑ ↓:    見出しの移動
    ;;     M-S-Enter:    見出しの追加
    ;;     M-Enter:      リストの追加
    ;;     C-c C-d:      TODOの追加(トグル)
    ;;     C-c ':        コードブロックでmode編集
    ;;     C-c C-x ENTER バッファ内で整形表示
    ;;     C-c C-c p     ブラウザで表示
    ;; ------------------------------
    :req "emacs-25.1"
    :tag "itex" "github flavored markdown" "markdown" "emacs>=25.1"
    :url "https://jblevins.org/projects/markdown-mode/"
    :added "2021-09-15"
    :emacs>= 25.1
    :ensure t
    :leaf-defer t
    :mode ("\\.md\\'" . gfm-mode)
    :custom
    (markdown-command . "github-markup")
    (markdown-command-needs-filename . t)
    (markdown-preview-stylesheets . '(list "~/.emacs.d/css/github.css"))
    :config
    (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
  )
  (leaf markdown-preview-mode
    :ensure t)
)

(leaf yaml-mode
  :ensure t
  :leaf-defer t
  :mode ("\\.yaml\\'" . yaml-mode)
)

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :added "2021-09-22"
  :emacs>= 24.3
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :custom ((flycheck-display-errors-delay . 0.3))
  :config
  (leaf flycheck-inline
    :ensure t
    :hook (flycheck-mode-hook . flycheck-inline-mode))
  (leaf flycheck-color-mode-line
    :ensure t
    :hook (flycheck-mode-hook . flycheck-color-mode-line-mode))
)

(leaf python
  ;; :custom
  ;; (python-indent-guess-indent-offset-verbose . nil)
  :config
  ;; (leaf python-mode
  ;;   :doc "Python major mode"
  ;;   :tag "oop" "python" "processes" "languages"
  ;;   :url "https://gitlab.com/groups/python-mode-devs"
  ;;   :added "2021-09-22"
  ;;   :ensure t
  ;;   :require t
  ;; )
  (leaf elpy
    :doc "Emacs Python Development Environment"
    :req "company-0.9.2" "emacs-24.4" "highlight-indentation-0.5.0" "pyvenv-1.3" "yasnippet-0.8.0" "s-1.11.0"
    :tag "tools" "languages" "ide" "python" "emacs>=24.4"
    :url "https://github.com/jorgenschaefer/elpy"
    :added "2021-09-15"
    :emacs>= 24.4
    :ensure t
    ;:after python-mode company highlight-indentation pyvenv yasnippet
    :init
    (elpy-enable)
    :custom
    ((elpy-rpc-backend . "jedi")) ; or 'jedi'
    :preface
    (defun exec-python nil
      "Use compile to run python programs"
      (interactive)
    (compile (concat "python " (buffer-file-name))))
    :hook
    ((python-mode . elpy-enable)
     (elpy-mode-hook . flycheck-mode))
    :config
    (remove-hook 'elpy-modules 'elpy-module-highlight-indentation) ;; インデントハイライトの無効化
    (remove-hook 'elpy-modules 'elpy-module-flymake) ;; flymakeの無効化
    (add-hook 'elpy-mode-hook (lambda ()
                                (auto-complete-mode -1)
                                (py-yapf-enable-on-save)
                               ;(define-key elpy-mode-map "\C-c\C-c" 'exec-python)
                               ;(highlight-indentation-mode -1))
                                ))
  )
  (leaf pipenv
    :doc "A Pipenv porcelain"
    :req "emacs-25.1" "s-1.12.0" "pyvenv-1.20"
    :tag "emacs>=25.1"
    :url "https://github.com/pwalsh/pipenv.el"
    :added "2021-09-22"
    :emacs>= 25.1
    :ensure t
    :after pyvenv python
    :require t
    :defvar python-shell-interpreter python-shell-interpreter-args python-shell-virtualenv-root pyvenv-activate
    :defun pipenv--force-wait pipenv-deactivate pipenv-projectile-after-switch-extended pipenv-venv
    :custom
    (pipenv-projectile-after-switch-function . #'pipenv-projectile-after-switch-extended)
    :init
    (defun pipenv-auto-activate ()
      (pipenv-deactivate)
      (pipenv--force-wait (pipenv-venv))
      (when python-shell-virtualenv-root
        (setq-local pyvenv-activate (directory-file-name python-shell-virtualenv-root))
        (setq-local python-shell-interpreter "pipenv")
        (setq-local python-shell-interpreter-args "run python")
      ))
    :hook (elpy-mode-hook . pipenv-auto-activate)
    :config
    (pyvenv-tracking-mode)
    (add-to-list 'python-shell-completion-native-disabled-interpreters "pipenv")
  )
  (leaf ein :ensure t)
)

(leaf fuzzy
  :doc "Fuzzy Matching"
  :req "emacs-24.3"
  :tag "convenience" "emacs>=24.3"
  :url "https://github.com/auto-complete/fuzzy-el"
  :added "2021-09-22"
  :emacs>= 24.3
  :ensure t
  :require t
)

(leaf py-yapf
  :doc "Use yapf to beautify a Python buffer"
  :url "https://github.com/paetzke/py-yapf.el"
  :added "2021-09-15"
  :ensure t
  :require t
  :config
  (add-hook 'python-mode-hook 'py-yapf-enable-on-save)
)

(leaf gui-settings
  :if window-system
  :config
  (modify-frame-parameters nil '((sticky . t) (width . 100) (height . 40)))
  (set-face-attribute 'default nil :family "RobotoJ Mono" :height 120) ; font

  ;; hl-line-mode
  (global-hl-line-mode 1)
  (set-face-background 'highlight "#333")
  (set-face-foreground 'highlight nil)
  (set-face-underline  'highlight t)
)

(leaf mozc
  ;; for flicker in bellow
  ;; -> xset -r 49
  :doc "minor mode to input Japanese with Mozc"
  :tag "input method" "multilingual" "mule"
  :added "2021-09-15"
  :ensure t
  :require t
  :custom
  ((default-input-method . "japanese-mozc")
  )
  :config
  (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
)

(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth"
  :tag "tools" "lisp" "convenience" "faces"
  :url "https://github.com/Fanael/rainbow-delimiters"
  :added "2021-09-15"
  :ensure t
  :leaf-defer t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode)
)

(leaf fontawesome
  :doc "fontawesome utility"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :url "https://github.com/syohex/emacs-fontawesome"
  :added "2021-09-15"
  :emacs>= 24.4
  :ensure t)

(leaf codic
  :doc "Search Codic (codic.jp) naming dictionaries"
  :req "emacs-24" "cl-lib-0.5"
  :tag "emacs>=24"
  :url "https://github.com/syohex/emacs-codic"
  :added "2021-09-15"
  :emacs>= 24
  :ensure t
  :leaf-defer t)

(leaf whitespace
  :doc "minor mode to visualize TAB, (HARD) SPACE, NEWLINE"
  :tag "builtin"
  :added "2021-09-15"
  :require t
  :custom
  ((whitespace-style . '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         ;empty         ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark))

   (whitespace-display-mappings . '((space-mark ?\u3000 [?\u25a1])
                                    ;; WARNING: the mapping below has a problem.
                                    ;; When a TAB occupies exactly one column, it will display the
                                    ;; character ?\xBB at that column followed by a TAB which goes to
                                    ;; the next TAB column.
                                    ;; If this is a problem for you, please, comment the line below.
                                    (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
   (whitespace-space-regexp . "\\(\u3000+\\)")
  )
  :config
  (global-whitespace-mode 1)
  (defvar my/bg-color "#282a36")
  (set-face-attribute 'whitespace-trailing nil
                      :background my/bg-color
                      :foreground "DeepPink"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background my/bg-color
                                        ;:foreground "LightSkyBlue"
                      :underline t)
  (set-face-attribute 'whitespace-space nil
                      :background my/bg-color
                      :foreground "GreenYellow"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                      :background my/bg-color)
)

(leaf modus-themes
  :doc "Highly accessible themes (WCAG AAA)"
  :req "emacs-27.1"
  :tag "accessibility" "theme" "faces" "emacs>=27.1"
  :url "https://gitlab.com/protesilaos/modus-themes"
  :added "2021-09-14"
  :emacs>= 27.1
  :ensure t
  :require t
  :custom
  ((modus-themes-slanted-constructs . t)
   (modus-themes-no-mixed-fonts . t)
   (modus-themes-subtle-line-numbers . t)
   (modus-themes-mode-line . '(moody borderless))
   (modus-themes-syntax . 'faint)
   (modus-themes-paren-match . 'intense-bold)
   (modus-themes-region . 'bg-only)
   (modus-themes-diffs . 'deuteranopia)
   (modus-themes-org-blocks . 'gray-background)
   ;; (modus-themes-variable-pitch-ui . t)
   ;; (modus-themes-variable-pitch-headings . t)
   (modus-themes-scale-headings . t)

   ;; (modus-themes-scale-1 . 1.1)
   ;; (modus-themes-scale-2 . 1.15)
   ;; (modus-themes-scale-3 . 1.21)
   ;; (modus-themes-scale-4 . 1.27)
   ;; (modus-themes-scale-title . 1.33)
  )
  :config
  (modus-themes-load-themes)
  (modus-themes-load-vivendi)
)

;; (leaf doom-modeline
;;   :doc "A minimal and modern mode-line"
;;   :req "emacs-25.1" "all-the-icons-2.2.0" "shrink-path-0.2.0" "dash-2.11.0"
;;   :tag "mode-line" "faces" "emacs>=25.1"
;;   :url "https://github.com/seagle0128/doom-modeline"
;;   :added "2021-09-16"
;;   :emacs>= 25.1
;;   :ensure t
;;   :require t
;;   :after all-the-icons shrink-path
;;   :config
;;   (doom-modeline-mode 1)
;; )

(leaf moody
  :doc "Tabs and ribbons for the mode line"
  :req "emacs-25.3"
  :tag "emacs>=25.3"
  :url "https://github.com/tarsius/moody"
  :added "2021-09-16"
  :emacs>= 25.3
  :ensure t
  :custom
  ((x-underline-at-descent-line . t))
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
)

(leaf minions
  :doc "A minor-mode menu for the mode line"
  :req "emacs-25.2"
  :tag "emacs>=25.2"
  :url "https://github.com/tarsius/minions"
  :added "2021-09-16"
  :emacs>= 25.2
  :ensure t
  :require t
  :init
  (minions-mode)
  :custom
  ((minions-mode-line-lighter . "[+]"))
)

;; My Settings ends
;; ----

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
