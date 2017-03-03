;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     nginx
     ansible
     javascript
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     emacs-lisp
     git
     markdown
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     version-control
     (python :variables
             python-test-runner 'pytest)
     ipython-notebook
     shell-scripts
     github
     yaml
     latex
     tmux
     ;; mu4e
     html
     windows-scripts
     ;; haskell
     ;; csharp
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(pkgbuild-mode
                                      cl-generic
                                      rst
                                      editorconfig
                                      polymode
                                      org-caldav
                                      rudel
                                      floobits)


   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(smartparens)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         monokai
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;;
   exec-path-from-shell-check-startup-files nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  ;; Change shell so grabbing envs works (Change it back later)
  (setenv "SHELL" "/usr/bin/zsh")
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;; Shortcut to start a terminal
  (spacemacs/set-leader-keys "at" 'ansi-term)
  ;; Use K and J to page up and page down
  (define-key evil-normal-state-map (kbd "K") 'scroll-down-command )
  (define-key evil-normal-state-map (kbd "J") 'scroll-up-command )
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; Set default notebook port to be my background jupyter server
  (setq-default
   ein:default-url-or-port "8215")

  ;; PythonTeX Polymode
  ;;;;;;;;;;;;;;;;;;;;;
  (setq load-path
        (append '("~/.config/spacemacs/.polymode/")
                load-path))
  (require 'poly-pythontex)

  ;; Disable automatic enable
  ;; (add-to-list 'auto-mode-alist '("\\.tex" . poly-pythontex-mode))


  ;;;;;;;;;;;;;;
  ;; Org Mode ;;
  ;;;;;;;;;;;;;;

  ;; Global Key Bindings
  ;;;;;;;;;;;;;;;;;;;;;;
  (spacemacs/declare-prefix "o" "Org Mode Global")
  (spacemacs/set-leader-keys "oa" 'org-agenda)
  (spacemacs/set-leader-keys "ot" (lambda() (interactive)(find-file "~/Notebooks/ToDo.org")))
  (spacemacs/set-leader-keys "oh" 'org-insert-todo-heading)
  (spacemacs/set-leader-keys "os" 'org-insert-todo-subheading)
  (spacemacs/set-leader-keys "oc" 'org-capture)
  (with-eval-after-load 'org

    (require 'org-capture)
    (require 'org-protocol)
    (require 'org-habit)


    (setq org-startup-truncated nil)
    (load-file (expand-file-name "~/.config/spacemacs/bh-org.el"))
    (load-file (expand-file-name "~/.config/spacemacs/cadair-org-mode.el"))

    ;; End Org Mode Section
    )

  ;; ORG MODE CALENDAR
  ;;;;;;;;;;;;;;;;;;;;
  (require 'org-caldav)

  ;; The CalDAV URL with your full and primary email address at the end.
  (setq org-caldav-url "https://apps.kolabnow.com/calendars/stuart%40cadair.com")

  ;; The name of your calendar, typically "Calendar" or similar
  (setq org-caldav-calendar-id "26ad9118-3d38-4676-9d31-cb8f09d9b730")

  ;; Local file that gets events from the server
  (setq org-caldav-inbox "~/Notebooks/calendar.org")

  ;; List of your org files here
  (setq org-caldav-files org-agenda-files)

  ;; Please make sure to set your correct timezone here
  (setq org-icalendar-timezone "Europe/London")
  (setq org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S")

  ;; GPG Encryption
  ;;;;;;;;;;;;;;;;;
  (require 'epa-file)
  (epa-file-enable)



  ;; Conda
  ;;;;;;;;
  (defun setenviron (envname) (interactive "sEnivronment name: ")
          (let(
              (envpath (concat "/opt/miniconda/envs/" envname))
              )
          (setenv "PATH" (concat (concat envpath "/bin:") (getenv "PATH")))
          (setq exec-path (append exec-path '(concat envpath "/bin:")))
          (pythonic-activate envpath)
          )
          )
  (spacemacs/set-leader-keys "ae" 'setenviron)
  (setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt --pprint")
  (setq python-shell-completion-native-disabled-interpreters '("ipython"))
  ;; TEST: Make TAB use company
  ;;(global-set-key "\t" 'company-complete-common)
  (setenv "SHELL" "/usr/bin/xonsh")

 )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol t)
 '(magit-pull-arguments nil)
 '(org-agenda-files
   (quote
    ("~/Notebooks/DKIST.org" "~/Notebooks/DKISTWP1.org" "~/Notebooks/PyAstro17.org" "~/Notebooks/SunPy.org" "~/Notebooks/ToDo.org" "~/Notebooks/calendar.org" "~/Notebooks/refile.org")))
 '(package-selected-packages
   (quote
    (floobits rudel nginx-mode jinja2-mode ansible-doc ansible web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode insert-shebang hide-comnt pug-mode org goto-chg diminish yapfify uuidgen py-isort org-projectile org-download mwim mu4e-maildirs-extension mu4e-alert live-py-mode link-hint github-search flyspell-correct-helm flyspell-correct eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff eshell-z dumb-jump company-shell column-enforce-mode undo-tree yaml-mode xterm-color ws-butler window-numbering web-mode volatile-highlights vi-tilde-fringe toc-org tagedit spacemacs-theme spaceline powerline smooth-scrolling smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-delimiters pyvenv pytest pyenv-mode py-yapf powershell popwin polymode pkgbuild-mode pip-requirements persp-mode pcre2el paradox hydra spinner page-break-lines orgit org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-caldav org-bullets open-junk-file neotree multi-term move-text mmm-mode markdown-toc markdown-mode magit-gitflow magit-gh-pulls macrostep lorem-ipsum linum-relative leuven-theme less-css-mode jade-mode info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile helm-gitignore helm-flyspell helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio gnuplot gitignore-mode github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md flycheck-pos-tip flycheck pkg-info epl flx-ido flx fish-mode fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-args evil-anzu anzu eval-sexp-fu highlight eshell-prompt-extras esh-help emmet-mode elisp-slime-nav ein request websocket editorconfig diff-hl define-word cython-mode company-web web-completion-data company-statistics company-quickhelp pos-tip company-auctex company-anaconda company clean-aindent-mode buffer-move bracketed-paste auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed auctex-latexmk auctex anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup quelpa package-build use-package which-key bind-key bind-map evil monokai-theme)))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(org-done ((t (:weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16)) (:strike-through t)))))
