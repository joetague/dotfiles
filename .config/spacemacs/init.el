;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/asciidoc/README.org
     ;; asciidoc

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+completion/auto-completion/README.org
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-minimum-prefix-length 1
                      auto-completion-idle-delay 0.2
                      auto-completion-private-snippets-directory nil
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-use-company-box nil
                      auto-completion-enable-sort-by-usage t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/clojure/README.org
     (clojure :variables
              clojure-backend 'cider
              clojure-enable-linters 'clj-kondo
              clojure-toplevel-inside-comment-form t
              cider-overlays-use-font-lock t
              clojure-enable-clj-refactor t
              cider-repl-buffer-size-limit 100)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Bcompletion/compleseus
     ;; (compleseus :variables
     ;;             compleseus-engine 'vertico)

     ;; copy-as-format for pasting code blocks into various things like GitHub/Slack/JIRA as formatted code
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+misc/copy-as-format/README.org
     ;; https://github.com/sshaw/copy-as-format
     copy-as-format

     ;; Tools to work with comma separate values
     ;; Used for data science files
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/csv/README.org
     csv

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/dap/README.org
     ;; dap

     ;; Don't autoload docsets use dir.locals.el to load appropriate ones per project
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+readers/dash/README.org
     (dash :variables
           dash-autoload-common-docsets nil)

     ;; Introduces a clash now with builtin TRAMP support
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/docker/README.org
     (docker :variables
             docker-dockerfile-backend 'lsp)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+readers/elfeed/README.org
     (elfeed :variables
             elfeed-db-directory "~/org/feeds/.elfeed"
             rmh-elfeed-org-files (list "~/org/feeds/feeds.org"))

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/emacs-lisp/README.org
     emacs-lisp

     ;; Include emojis into everything
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+fun/emoji/README.org
     emoji

     ;; SPC g s opens Magit git client full screen (q restores previous layout)
     ;; refine hunk 'all highlights characters changed on each line
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+source-control/git/README.org
     (git :variables
          git-magit-status-fullscreen t
          magit-diff-refine-hunk 'all)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/go/README.org
     ;; brew install gopls golangci-lint
     (go :variables
         go-backend 'lsp
         go-format-before-save t
         go-tab-width 2
         go-use-golangci-lint t
         go-use-testify-for-testing t)

     ;; graphviz - open-source graph declaration system
     ;; Used to generated graphs of Clojure project dependencies
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/graphviz/README.org
     ;; graphviz

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+completion/helm/README.org
     (helm :variables
           helm-buffer-max-length 60
           helm-enable-auto-resize t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/html/README.org
     html

     ;; Just use IntelliJ?
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/java/README.org
     ;; (java :variables
     ;;      java-backend 'lsp)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/javascript/README.org
     ;; (javascript :variables
     ;;             javascript-backend 'lsp
     ;;             javascript-lsp-linter nil
     ;;             js2-mode-show-strict-warnings nil
     ;;             js2-mode-show-parse-errors nil)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/json/README.org
     (json :variables
           json-fmt-on-save nil)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/kotlin/README.org
     ;; (kotlin :variables
     ;;         kotlin-backend 'lsp
     ;;         kotlin-lsp-jar-path "/usr/local/bin/kotlin-language-server")

     ;; Language server protocol with minimal visual impact
     ;; https://practicalli.github.io/spacemacs/install-spacemacs/clojure-lsp/lsp-variables-reference.html
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/lsp/README.org
     (lsp :variables
          ;; Do not install lsp-ui package
          lsp-use-lsp-ui nil
          ;; Formatting and indentation - use Cider instead
          lsp-enable-on-type-formatting t
          ;; Set to nil to use CIDER features instead of LSP UI
          lsp-enable-indentation nil
          lsp-enable-snippet nil  ;; to test again

          ;; symbol highlighting - `lsp-toggle-symbol-highlight` toggles highlighting
          ;; subtle highlighting for doom-gruvbox-light theme defined in dotspacemacs/user-config
          lsp-enable-symbol-highlighting t

          ;; Show lint error indicator in the mode line
          lsp-modeline-diagnostics-enable t
          ;; lsp-modeline-diagnostics-scope :workspace

          ;; popup documentation boxes
          lsp-ui-doc-enable nil          ;; disable all doc popups
          ;; lsp-ui-doc-show-with-cursor nil   ;; doc popup for cursor
          ;; lsp-ui-doc-show-with-mouse t   ;; doc popup for mouse
          ;; lsp-ui-doc-delay 2                ;; delay in seconds for popup to display
          ;; lsp-ui-doc-include-signature t    ;; include function signature
          ;; lsp-ui-doc-position 'at-point  ;; top bottom at-point
          lsp-ui-doc-alignment 'window      ;; frame window

          ;; code actions and diagnostics text as right-hand side of buffer
          lsp-ui-sideline-enable nil
          lsp-ui-sideline-show-code-actions nil
          ;; lsp-ui-sideline-delay 500
          ;; lsp-ui-sideline-show-diagnostics nil

          ;; reference count for functions (assume their maybe other lenses in future)
          lsp-lens-enable t

          ;; Efficient use of space in treemacs-lsp display
          treemacs-space-between-root-nodes nil
          lsp-treemacs-sync-mode 1
          lsp-treemacs-error-list-current-project-only t  ; limit errors to current project

          ;; Optimization for large files
          lsp-file-watch-threshold 10000
          lsp-log-io nil)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/markdown/README.org
     markdown

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+misc/multiple-cursors/README.org
     (multiple-cursors :variables
                       multiple-cursors-backend 'evil-mc)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+emacs/org/README.org
     ;; Do not use current org-journal from syl20bnr packages.el nor the by patching the emacs file back to bastibe version
     ;; use forked at joetague (see additional packages)
     (org :variables
          org-enable-github-support t
          org-enable-org-journal-support t
          org-enable-valign t
          org-enable-verb-support t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+os/osx/README.org
     osx

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+readers/pdf/README.org
     pdf

     ;; Currently got plantuml in a container, needs non-alias executable - not sure bassh script will cut it
     ;; alternative is to install via homebrew and reference plantuml-jar-path
     ;; (plantuml :variables
     ;;           plantuml-jar-path "/usr/local/Cellar/plantuml/1.2022.7/libexec/plantuml.jar"
     ;;           org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2022.7/libexec/plantuml.jar")

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/protobuf/README.org
     ;; protobuf

     ;; Just use IntelliJ with plugins?
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/python/README.org
     (python :variables
             python-backend 'lsp
             python-lsp-server 'pyright
             python-auto-set-local-pyenv-version 'on-project-switch
             python-auto-set-local-pyvenv-virtualenv 'on-project-switch
             python-fill-column 120
             python-formatter 'black
             python-format-on-save t
             python-sort-imports-on-save t
             python-test-runner 'pytest
             )

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/restclient/README.org
     ;; restclient

     ;; Just use VSCode?
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+frameworks/react/README.org
     ;; react

     ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+lang/rust/README.org
     (rust :variables
           lsp-rust-analyzer-cargo-auto-reload t
           rustic-format-on-save t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/scala/README.org
     ;; (scala :variables
     ;;        scala-backend 'scala-metals
     ;;        scala-auto-start-backend t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/shell/README.org
     (shell :variables
            shell-default-shell 'vterm
            shell-default-height 30
            shell-default-position 'bottom
            spacemacs-vterm-history-file-location "~/.zsh_history")

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/shell-scripts/README.org
     shell-scripts

     ;; Just use Slack client? Although nice to be able to capture
     ;; from slack buffers with direct link back for notes/journal
     ;; MAIN PACKAGE DEPRECATED: https://github.com/yuya373/emacs-slack
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+chat/slack/README.org
     ;; (slack :variables
     ;;        slack-spacemacs-layout-name "@Slack"
     ;;        slack-spacemacs-layout-binding "s")

     ;; spacemacs-layouts layer added to set variables
     ;; SPC TAB restricted to current layout buffers
     ;; Kill buffers when killing layer - SPC l x
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+spacemacs/spacemacs-layouts/README.org
     (spacemacs-layouts :variables
                        spacemacs-layouts-restrict-spc-tab t
                        persp-autokill-buffer-on-remove t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+checkers/spell-checking/README.org
     spell-checking

     ;; Just use the Spotify client - this controls via applescript with some web sdk lookups but it's really distracting
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+music/spotify/README.org
     ;; (spotify :variables
     ;;          counsel-spotify-client-id "c9d5094ef2894115836951340b68dfe7"
     ;;          counsel-spotify-client-secret (auth-source-pick-first-password
     ;;                                         :host "api.spotify.com"
     ;;                                         :user "joetague"))

     ;; Not used this in anger yet
     ;; Alternative might be: https://github.com/kostafey/ejc-sql
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/sql/README.org
     ;; (sql :variables
     ;;      sql-backend 'lsp
     ;;      sql-lsp-sqls-workspace-config-path 'workspace
     ;;      sql-capitalize-keywords t
     ;;      sql-auto-indent nil)

     ;; Original flycheck fringe bitmaps is deprecated
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+checkers/syntax-checking/README.org
     ;; syntax-checking-indication-symbol '(nil . nil)
     (syntax-checking :variables
                      flycheck-indication-mode nil)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/terraform/README.org
     ;; https://github.com/hashicorp/terraform-ls
     ;; Needs: brew install hashicorp/tap/terraform-ls
     (terraform :variables
                terraform-auto-format-on-save t
                terraform-backend 'lsp)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+filetree/treemacs/README.org
     (treemacs :variables
               treemacs-indentation 1
               treemacs-use-filewatch-mode t
               treemacs-use-follow-mode t
               treemacs-use-scope-type 'Perspectives)

     (typescript :variables
                 tide-tsserver-executable "/opt/homebrew/bin/tsserver")

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+source-control/version-control/README.org
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/yaml/README.org
     yaml
     )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      casual-agenda
                                      casual-calc
                                      casual-dired
                                      casual-isearch
                                      eglot
                                      elfeed-tube
                                      elfeed-tube-mpv
                                      ;; org-timeblock
                                      ;; org-incoming ;; ingest PDF files into your org or org-roam files.
                                      ;; org-sort-tasks ;; sort an unsorted TODO list using mergesort
                                      org-ql
                                      ;; org-mru-clock
                                      jet
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 3 (* 1024 1024))

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 3)
                                (projects . 5))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.2

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.3

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  ;; Disable undo-tree as it slows everything down
  ;; (global-undo-tree-mode -1)
  (setq evil-undo-system 'undo-redo)
  (evil-set-undo-system 'undo-redo)

  ;; Emacs text rendering optimisations
  ;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
  ;; Only render text left to right
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Fixes GPG setup
  ;; Set the files that are searched for writing tokens
  ;; by default ~/.authinfo will be used
  ;; and write a token in unencrypted format
  (setq auth-sources '("~/.authinfo.gpg"))

  ;; Magit - forge configuration

  ;; Configure number of topics show, open and closed
  ;; use negative number to toggle the view of closed topics
  ;; using `SPC SPC forge-toggle-closed-visibility'
  (setq  forge-topic-list-limit '(100 . -5))
  ;; set closed to 0 to never show closed issues
  ;; (setq  forge-topic-list-limit '(100 . 0))
  ;;
  ;; GitHub user and organization accounts owned
  ;; used by @ c f  to create a fork
  (setq forge-owned-accounts
        '(("joetague")))
  (setq magit-repository-directories
        '(("~/.emacs.d"  . 0)
          ("~/proj/" . 4)))

  ;; Setup tree-sitter grammars
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (regex "https://github.com/tree-sitter/tree-sitter-regex")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (clojure "https://github.com/sogaiu/tree-sitter-clojure")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
          (org "https://github.com/milisims/tree-sitter-org")
          (proto "https://github.com/mitchellh/tree-sitter-proto")
          ))

  ;; TODO Checkout other interesting setup here: https://github.com/dakra/dmacs/blob/master/init.org

  ;; Projectile
  (setq projectile-create-missing-test-files t)
  (with-eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

  ;; Setup org
  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-protocol)
    ;; (add-to-list 'org-modules 'org-tempo)

    (require 'casual-agenda) ; optional
    (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)

    (setq org-confirm-babel-evaluate '(not (y-or-n-p "evaluate block? ")))

    (setq org-agenda-files '("~/org/learning.org"))
    (setq org-projectile-file "~/org/projectile/projects.org")
    (setq org-agenda-files (append org-agenda-files '("~/org/projectile/projects.org")))

    (setq org-journal-dir "~/org/journal/")
    (setq org-journal-file-format "%Y%m%d")
    (setq org-journal-encrypt-journal t)

    (setq org-noter-default-notes-file-names '("~/org/learning.org")
          org-noter-notes-search-path '("~/org"))

    (setq org-persp-startup-with-agenda "z")

    (setq org-deadline-warning-days 5)
    (setq org-clock-into-drawer t)
    (setq org-clock-persist t)
    (setq spaceline-org-clock-p t)
    (setq org-columns-default-format "%60ITEM(Task) %20TODO %10Effort(Effort){:} %10CLOCKSUM")
    (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                        ("STYLE_ALL" . "habit"))))
    (setq org-duration-format '((special . h:mm)))
    (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
    (setq org-icalendar-timezone "Europe/London")

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((clojure . t)
       (emacs-lisp . t)
       (dot . t)
       (js . t)
       (python . t)
       (shell . t)
       (sqlite . t)
       ))
    ) ;; end with-eval-after-load

  ;; Elfeed settings
  (with-eval-after-load 'elfeed
    (require 'elfeed-tube)
    (elfeed-tube-setup)
    (define-key elfeed-show-mode-map (kbd "F") 'elfeed-tube-fetch)
    (define-key elfeed-show-mode-map [remap save-buffer] 'elfeed-tube-save)
    (define-key elfeed-search-mode-map (kbd "F") 'elfeed-tube-fetch)
    (define-key elfeed-search-mode-map [remap save-buffer] 'elfeed-tube-save))

  ;; Casual Suite setup for tools
  ;; see: https://github.com/kickingvegas/casual-suite settings
  (require 'casual-calc) ;; optional
  (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
  (keymap-set calc-alg-map "C-o" #'casual-calc-tmenu)

  (require 'casual-dired)
  (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
  (keymap-set dired-mode-map "s" #'casual-dired-sort-by-tmenu) ; optional
  (keymap-set dired-mode-map "/" #'casual-dired-search-replace-tmenu) ; optional

  (require 'casual-isearch)
  (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)

  ;; Projectile settings
  ;; See: https://github.com/syl20bnr/spacemacs/issues/4207 should improve speed
  ;; of helm-projectile by using a shell that doesn't have a lot of profile information
  ;; Previously tried
  ;; (setq shell-file-name "/bin/sh")
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'native)
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("/Users/joetague/org/learning.org"))
 '(package-selected-packages
   '(lsp-mode helm-swoop yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode winum which-key web-beautify volatile-highlights vim-powerline vi-tilde-fringe valign uuidgen undo-tree treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toc-org terminal-here term-cursor symon symbol-overlay string-inflection string-edit-at-point sphinx-doc spacemacs-whitespace-cleanup spacemacs-purpose-popwin spaceline space-doc smeargle shfmt shell-pop restart-emacs request rainbow-delimiters quickrun pytest pylookup pyenv-mode pydoc py-isort prettier-js poetry pippel pipenv pip-requirements pdf-view-restore pcre2el password-generator paradox ox-gfm overseer orgit-forge org-superstar org-rich-yank org-projectile org-present org-pomodoro org-mime org-journal org-download org-contrib org-cliplink open-junk-file nose nameless multi-vterm multi-term multi-line markdown-toc macrostep lsp-python-ms lsp-pyright lsp-origami lorem-ipsum live-py-mode json-reformat json-navigator json-mode inspector insert-shebang info+ indent-guide importmagic hybrid-mode hungry-delete htmlize holy-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt helm-xref helm-themes helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-git-grep helm-descbinds helm-dash helm-company helm-comint helm-cider helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitignore-templates git-timemachine git-modes git-messenger git-link gh-md flyspell-correct-helm flycheck-pos-tip flycheck-package flycheck-elsa flycheck-clj-kondo flycheck-bashate flx-ido fish-mode fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-evilified-state evil-escape evil-easymotion evil-collection evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help emr emojify emoji-cheat-sheet-plus elisp-slime-nav elisp-def elfeed-org elfeed-goodies editorconfig dumb-jump drag-stuff dotenv-mode dockerfile-mode docker dired-quick-sort diminish diff-hl devdocs define-word dash-at-point dap-mode cython-mode csv-mode copy-as-format company-statistics company-shell company-quickhelp company-emoji company-anaconda column-enforce-mode code-cells clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu centered-cursor-mode browse-at-remote blacken auto-highlight-symbol auto-dictionary auto-compile all-the-icons aggressive-indent ace-link ace-jump-helm-line))
 '(safe-local-variable-values '((python-shell-interpreter . "ipython"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
