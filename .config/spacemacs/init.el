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
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/aider
     (aider)
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

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/claude-code
     (claude-code :variables
                  claude-code-ide-window-side 'right
                  claude-code-ide-window-width 100)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/clojure/README.org
     (clojure :variables
              clojure-backend 'cider
              clojure-enable-linters '(clj-kondo joker)
              clojure-enable-clj-refactor t
              clojure-enable-kaocha-runner t
              clojure-toplevel-inside-comment-form t)

     ;; Alternative to Helm
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Bcompletion/compleseus
     (compleseus :variables
                 compleseus-engine 'vertico
                 compleseus-use-nerd-icons t)

     ;; copy-as-format for pasting code blocks into various things like GitHub/Slack/JIRA as formatted code
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+misc/copy-as-format/README.org
     ;; https://github.com/sshaw/copy-as-format
     copy-as-format

     ;; Tools to work with comma separate values
     ;; Used for data science files
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/csv/README.org
     csv

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/dap/README.org
     dap

     ;; Don't autoload docsets use dir.locals.el to load appropriate ones per project
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+readers/dash/README.org
     ;; (dash :variables
     ;;       dash-autoload-common-docsets nil)

     ;; Introduces a clash now with builtin TRAMP support
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/docker/README.org
     (docker :variables
             docker-dockerfile-backend 'lsp)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+readers/elfeed/README.org
     (elfeed :variables
             elfeed-db-directory "~/org/feeds/.elfeed"
             rmh-elfeed-org-files (list "~/org/elfeed.org"))

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/emacs-lisp/README.org
     emacs-lisp

     ;; Include emojis into everything
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+fun/emoji/README.org
     emoji

     ;; SPC g s opens Magit git client full screen (q restores previous layout)
     ;; refine hunk 'all highlights characters changed on each line
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+source-control/git/README.org
     (git :variables
          git-magit-status-fullscreen t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/go/README.org
     ;; brew install gopls golangci-lint
     ;; (go :variables
     ;;     go-backend 'lsp
     ;;     go-format-before-save t
     ;;     go-tab-width 2
     ;;     go-use-golangci-lint t
     ;;     go-use-testify-for-testing t)

     ;; graphviz - open-source graph declaration system
     ;; Used to generated graphs of Clojure project dependencies
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/graphviz/README.org
     ;; graphviz

     ;; Prefer compleseus?
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+completion/helm/README.org
     ;; (helm :variables
     ;;       helm-buffer-max-length 60
     ;;       helm-enable-auto-resize t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/html/README.org
     html

     ;; Just use IntelliJ?
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/java/README.org
     (java :variables
           java-backend 'lsp)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/javascript/README.org
     (javascript :variables
                 javascript-backend 'lsp
                 javascript-lsp-linter nil
                 js2-mode-show-strict-warnings nil
                 js2-mode-show-parse-errors nil)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/json/README.org
     (json :variables
           json-backend 'lsp
           json-fmt-on-save nil)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/kotlin/README.org
     ;; (kotlin :variables
     ;;         kotlin-backend 'lsp
     ;;         kotlin-lsp-jar-path "/usr/local/bin/kotlin-language-server")

     ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Btools/kubernetes/README.org
     kubernetes

     ;; Large Language Model
     ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Bweb-services/llm-client/README.org
     (llm-client :variables
                 llm-client-enable-ellama nil
                 llm-client-enable-gptel t)

     ;; Language server protocol with minimal visual impact
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/lsp/README.org
     (lsp :variables
          ;; Do not install lsp-ui package
          lsp-use-lsp-ui nil
          ;; Disable lsp-ui-doc overlay
          lsp-ui-doc-enable nil
          ;; Disable type signature included in the lsp-ui-doc overlay
          lsp-ui-doc-include-signature nil
          ;; Disable lsp-ui-sideline overlay
          lsp-ui-sideline-enable nil
          ;; Disable sideline includes symbol info
          lsp-ui-sideline-show-symbol nil
          ;; Ignore duplicates
          lsp-ui-sideline-ignore-duplicate t
          ;; Disable map keys to lsp-command-map
          lsp-use-upstream-bindings nil
          ;; Disable lsp-sonarlint package
          lsp-sonarlint nil)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/markdown/README.org
     ;; markdown

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+misc/multiple-cursors/README.org
     (multiple-cursors :variables
                       multiple-cursors-backend 'evil-mc)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+emacs/org/README.org
     (org :variables
          org-enable-github-support t
          org-enable-org-journal-support t
          org-enable-valign t
          org-enable-verb-support t
          org-persp-startup-org-file "~/org/life.org"
          org-project-capture-projects-file "~/org/projects.org")

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+os/osx/README.org
     (osx :variables
          osx-dictionary-dictionary-choice "English")

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+readers/pdf/README.org
     pdf

     ;; Currently got plantuml in a container, needs non-alias executable - not sure bassh script will cut it
     ;; alternative is to install via homebrew and reference plantuml-jar-path
     ;; (plantuml :variables
     ;;           plantuml-jar-path "/usr/local/Cellar/plantuml/1.2022.7/libexec/plantuml.jar"
     ;;           org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2022.7/libexec/plantuml.jar")

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/protobuf/README.org
     protobuf

     ;; Just use IntelliJ with plugins?
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/python/README.org
     (python :variables
             python-auto-set-local-pyenv-version 'on-project-switch
             python-auto-set-local-pyvenv-virtualenv 'on-project-switch
             python-backend 'lsp
             python-lsp-server 'pyright
             python-test-runner 'pytest)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/restclient/README.org
     ;; restclient

     ;; Just use VSCode?
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+frameworks/react/README.org
     ;; react

     ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+lang/rust/README.org
     ;; (rust :variables
     ;;       lsp-rust-analyzer-cargo-auto-reload t
     ;;       rustic-format-on-save t)

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
     (shell-scripts :variables
                    shell-scripts-backend 'lsp)

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
     ;; spell-checking

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
     ;; (terraform :variables
     ;;            terraform-auto-format-on-save t
     ;;            terraform-backend 'lsp)


     ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+completion/templates/README.org
     templates

     ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+lang/toml/README.org
     toml

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+filetree/treemacs/README.org
     (treemacs :variables
               treemacs-indentation 1
               treemacs-use-filewatch-mode t
               treemacs-use-follow-mode t
               treemacs-use-scope-type 'Perspectives)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/tree-sitter/README.org
     ;; (tree-sitter :variables
     ;;              spacemacs-tree-sitter-hl-black-list '(js2-mode rjsx-mode)
     ;;              tree-sitter-syntax-highlight-enable t
     ;;              tree-sitter-fold-enable t
     ;;              tree-sitter-fold-indicators-enable t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Blang/typescript
     ;; (typescript :variables
     ;;             tide-tsserver-executable "/opt/homebrew/bin/tsserver")

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+source-control/version-control/README.org
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/yaml/README.org
     (yaml :variables
           yaml-enable-lsp t)
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
                                      ;; casual-suite
                                      (clay :location (recipe
                                                       :fetcher github
                                                       :repo "scicloj/clay.el"))
                                      clj-deps-new
                                      clojure-ts-mode
                                      consult-gh
                                      consult-gh-forge
                                      consult-gh-with-pr-review
                                      ;; consult-omni
                                      ;; elfeed-tube
                                      ;; elfeed-tube-mpv
                                      envrc
                                      ;; (gptel-project :location (recipe
                                      ;;                  :fetcher github
                                      ;;                  :repo "cvdub/gptel-project"))
                                      (gptel-quick :location (recipe
                                                              :fetcher github
                                                              :repo "karthink/gptel-quick"))
                                      ;; org-timeblock
                                      (jet :location (recipe
                                                      :fetcher github
                                                      :repo "ericdallo/jet.el"))
                                      (neil :location (recipe
                                                       :fetcher github
                                                       :repo "babashka/neil"))

                                      (ob-gptel :location (recipe
                                                           :fetcher github
                                                           :repo "jwiegley/ob-gptel"))
                                      ;; org-incoming ;; ingest PDF files into your org or org-roam files.
                                      ;; org-noter
                                      org-pdftools
                                      org-ql
                                      org-super-agenda
                                      ;; org-sort-tasks ;; sort an unsorted TODO list using mergesort
                                      ;; org-mru-clock
                                      quarto-mode
                                      symbol-overlay
                                      ;;yankpad
                                      ;;yatemplate
                                      ;;ya-org-capture
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
   dotspacemacs-read-process-output-max (* 1024 1024)

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
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "nerd-icons" package or the font
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
   ;; with 2 themes variants, one dark and one light). A theme from external
   ;; package can be defined with `:package', or a theme can be defined with
   ;; `:location' to download the theme package, refer the themes section in
   ;; DOCUMENTATION.org for the full theme specifications.
   dotspacemacs-themes '(modus-vivendi
                         modus-vivendi-deuteranopia
                         modus-vivendi-tinted
                         modus-vivendi-tritanopia)

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

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   ;; Default icons font, it can be `all-the-icons' or `nerd-icons'.
   dotspacemacs-default-icons-font 'all-the-icons

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
   ;; (default "C-M-m" for terminal mode, "M-<return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "M-<return>" "C-M-m")

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
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

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
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `ack' and `grep'.
   ;; (default '("rg" "ag" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "ack" "grep")

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

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
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
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
  ;; stable packages
  (add-to-list 'configuration-layer-elpa-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(transient . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(magit-popup . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(magit-section . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(forge . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(gptel . "melpa-stable") t)
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(defun jpt/delete-to-bol (delete-newline)
  "Delete current line and preceding newline char if DELETE-NEWLINE is set."
  (interactive "p")
  (delete-region (pos-bol) (pos-eol))
  (when delete-newline
    (delete-char -1)))

(defun jpt/move-line-up ()
  "Move current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun jpt/move-line-down ()
  "Move current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun jpt/duplicate-line ()
  "Duplicate current line and append under the current one."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(defun jpt/decode-jwt ()
  "Decode JWT that is on the current line."
  (interactive)
  (let* ((data (split-string (thing-at-point 'filename) "\\."))
         (header (car data))
         (claims (cadr data)))
    (with-temp-buffer
      (insert (format "%s\n\n%s"
                      (base64-decode-string header t)
                      (base64-decode-string claims t)))
      (json-pretty-print-buffer)
      (with-output-to-temp-buffer "*JWT*"
        (special-mode)
        (princ (buffer-string))))) t)

(defun jpt/generate-uuid ()
  "Insert a generated UUID at point."
  (interactive)
  (insert (downcase (string-trim (shell-command-to-string "uuidgen")))))

(defun jpt/projectile-kill-other-buffers ()
  "Kill all buffers in current project except for current buffer."
  (interactive)
  (let* ((current-buffer (current-buffer))
         (buffers (projectile-project-buffers)))
    (dolist (buffer buffers)
      (unless (eq buffer current-buffer)
        (kill-buffer buffer)))
    (message "Kill all buffers in project %s except %s"
             (projectile-project-name)
             (buffer-name current-buffer))))

(defun jpt/toggle-true-false ()
  "Toggle the word at point between `true' and `false'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word))
        (case-fold-search nil)) ;; Make search case-sensitive
    (when bounds
      (let* ((start (car bounds))
             (end (cdr bounds))
             (word (buffer-substring-no-properties start end)))
        (cond
         ((string-equal word "true") (delete-region start end) (insert "false"))
         ((string-equal word "false") (delete-region start end) (insert "true")))))))

(defun jpt/unix-timestamp-to-iso8601 ()
  "Convert Unix timestamp at point to an ISO8601 formatted date and display in minibuffer."
  (interactive)
  (let* ((timestamp (thing-at-point 'number t))
         (time (when timestamp (seconds-to-time timestamp))))
    (if time
        (message "ISO 8601 Date: %s" (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))
      (message "No valid Unix timestamp found at point."))))

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (setopt user-full-name "Joe Tague"
          user-mail-address "joetague@gmail.com")

  (setopt spacemacs-keep-legacy-current-buffer-delete-bindings nil)

  ;; Fixes GPG setup
  ;; Set the files that are searched for writing tokens
  ;; by default ~/.authinfo will be used
  ;; and write a token in unencrypted format
  (setopt auth-sources '("~/.authinfo.gpg"))

  ;; Always load newest byte code
  (setopt load-prefer-newer t)

  ;; Remove window decoration
  (setopt default-frame-alist '((undecorated-round . t)))
  ;; Disable undo-tree as it slows everything down
  ;; (global-undo-tree-mode -1)
  (envrc-global-mode)
  ;; (setq evil-undo-system 'undo-redo)
  ;; (evil-set-undo-system 'undo-redo)

  ;; Emacs text rendering optimisations
  ;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
  ;; Only render text left to right
  (setopt bidi-paragraph-direction 'left-to-right)


  ;; Clojure and tools
  (require 'clj-deps-new)
  (require 'jet)
  (require 'clay)
  (setopt neil-inject-dep-to-project-p t)
  (setopt neil-prompt-for-version-p t)

  ;; CIDER
  (setopt cider-overlays-use-font-lock t)
  (setopt cider-repl-buffer-size-limit 100)
  (setopt cider-repl-display-help-banner nil)
  ;; enable safe structural editing for all supported modes
  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)

  ;; def portal to the dev namespace to allow dereferencing via @dev/portal
  (defun portal.api/open ()
    (interactive)
    (cider-nrepl-sync-request:eval
     "(do (ns dev)
         (def portal ((requiring-resolve 'portal.api/open)))
         (add-tap (requiring-resolve 'portal.api/submit)))"))

  (defun portal.api/clear ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/clear)"))

  (defun portal.api/close ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/close)"))

  (spacemacs/declare-prefix-for-mode 'clojure-mode "dp" "Portal")
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "dpp" 'portal.api/open)
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "dpc" 'portal.api/clear)
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "dpD" 'portal.api/close)

  (spacemacs/declare-prefix-for-mode 'clojure-mode "ac" "Clay")
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "ach" 'clay-make-ns-html)
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "acq" 'clay-make-ns-quarto-html)
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "acr" 'clay-make-ns-quarto-revealjs)
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "acs" 'clay-make-last-sexp)
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "acf" 'clay-make-defun-at-point)

  ;; (defun copy-edn-as-json ()
  ;;   (interactive)
  ;;   (jet-to-clipboard
  ;;    (jet--thing-at-point)
  ;;    '("--from=edn" "--to=json"))
  ;;   (deactivate-mark))

  ;; (defun copy-json-as-edn ()
  ;;   (interactive)
  ;;   (jet-to-clipboard
  ;;    (jet--thing-at-point)
  ;;    '("--from=json" "--to=edn" "--keywordize"))
  ;;   (deactivate-mark))

  ;; (global-set-key (kbd "C-c j e j") 'copy-edn-as-json)
  ;; (global-set-key (kbd "C-c j j e") 'copy-json-as-edn)


  ;; Magit - forge configuration
  (setopt magit-diff-refine-hunk 'all)
  (setopt forge-topic-list-limit '(100 . -5))
  (setopt forge-owned-accounts
          '(("joetague")))
  (setopt magit-repository-directories
          '(("~/.emacs.d"  . 0)
            ("~/proj/" . 4)))


  ;; LSP - General and UI setup
  (with-eval-after-load 'lsp
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
    (setopt
     ;; Formatting and indentation - use Cider instead
     lsp-enable-on-type-formatting t
     ;; Set to nil to use CIDER features instead of LSP UI
     lsp-enable-indentation nil
     lsp-enable-snippet nil  ;; to test again

     ;; symbol highlighting - `lsp-toggle-symbol-highlight` toggles highlighting
     ;; subtle highlighting for doom-gruvbox-light theme defined in dotspacemacs/user-config
     lsp-enable-symbol-highlighting t

     ;; Show lint error indicator in the mode line
     lsp-modeline-diagnostics-enable nil

     ;; popup documentation boxes
     lsp-ui-doc-alignment 'window      ;; frame window

     ;; reference count for functions (assume their maybe other lenses in future)
     lsp-lens-enable t

     ;; Efficient use of space in treemacs-lsp display
     treemacs-space-between-root-nodes nil
     lsp-treemacs-sync-mode 1
     lsp-treemacs-error-list-current-project-only t  ; limit errors to current project

     ;; Optimization for large files
     lsp-file-watch-threshold 10000
     ;; Turn off as much deubg/logging to improve performance
     lsp-log-io nil
     ))

  ;; LSP mode - setup file wathcing limits to improve speed
  (with-eval-after-load 'lsp-mode
    ;; Addition ignores to suppliment common ones at: https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-mode.el#L344
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\build\\'"))

  ;; LSP - Java
  (with-eval-after-load 'lsp-java
    (setopt lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.49.0/jdt-language-server-1.49.0-202507311558.tar.gz")
    (setopt lsp-java-vmargs
            `("-XX:+UseParallelGC"
              "-XX:GCTimeRatio=4"
              "-XX:AdaptiveSizePolicyWeight=90"
              "-Dsun.zip.disableMemoryMapping=true"
              "-Xmx4G")

            lsp-java-progress-reports-enabled nil
            lsp-java-completion-max-results 50
            lsp-java-progress-reports nil
            lsp-java-autobuild-enabled nil)
    (setopt c-basic-offset 4
            tab-width 4
            indent-tabs-mode nil))

  ;; LSP - Python (pyright)
  (setopt lsp-pyright-multi-root nil)
  (with-eval-after-load 'lsp-pyright)

  ;; Org Mode
  (with-eval-after-load 'org
    ;; (add-to-list 'org-modules 'org-protocol)
    (add-hook 'completion-at-point-functions 'ob-gptel-capf nil t)

    ;; Crypt
    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    (setopt org-tags-exclude-from-inheritance (quote ("crypt")))
    (setopt org-crypt-key "F4E72EEA776B3FBC")
    ;; ;; GPG key to use for encryption
    ;; ;; Either the Key ID or set to nil to use symmetric encryption.
    (setopt auto-save-default nil)

    ;; ;; Capturing
    (setopt org-refile-targets '((nil :maxlevel . 4)
                                 (org-agenda-files :maxlevel . 4)))
    (setopt org-outline-path-complete-in-steps nil)         ; Refile in a single go
    (setopt org-refile-use-outline-path t)                  ; Show full paths for refiling

    ;; https://emacs.stackexchange.com/questions/12900/passing-a-variable-to-template-function-in-org-capture-templates
    ;; Creating a template for meeting notes
    (defvar my/org-meeting-template "** %<%Y-%m-%d %H:%M> %^{something} %(org-set-tags \"crypt\")
    SCHEDULED: %<%Y-%m-%d %H:%M>
    *Attendees:*
    - [X] Joe Tague
    - [ ] %?
    *Agenda:*
    -
    -
    *Notes:*
    " "Meeting Template")

    ;; Need tempaltes for the follwing types of meeting at work
    ;; 1-to-1
    ;; feedback
    ;; observation
    ;; Configure custom capture templates
    ;; Note the backtick here, it's required so that the defvar based tempaltes will work!
    ;; http://comments.gmane.org/gmane.emacs.orgmode/106890
    ;; (setq org-capture-templates
    ;;       `(
    ;;         ("t" "To-do" entry (file+headline "~/gdrive/org/work.org" "Inbox")
    ;;          "** TODO %^{Task Description}\nCreated From: %a\n" :clock-in t :clock-resume t :prepend t)

    ;;         ("m" "Meeting" entry (file+headline "~/gdrive/org/work.org" "Meetings"), my/org-meeting-template)
    ;;         ("m1" "1-to-1")
    ;;         ("m1J" "Me" entry (file+headline "~/gdrive/org/work.org" "Meetings"), my/org-meeting-template)

    ;;         ("f" "Feedback")
    ;;         ("fJ" "Joe" entry (file+olp "~/gdrive/org/work.org" "Feedback" "Bertrand"), my/org-feedback-template)

    ;;         ("p" "Protocol" entry (file+headline "~/gdrive/org/work.org" "Inbox")
    ;;          "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?")

    ;;         ("L" "Protocol Link" entry (file+headline "~/gdrive/org/work.org" "Inbox")
    ;;          "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
    ;;         ))

    ;; TODO Keywords
    (setopt org-todo-keywords
            (quote ((sequence "TODO(t)" "IN_PROGRESS(i)" "|" "DONE(d)")
                    (sequence "WAITING(w@/)" "HOLD(h@/)" "|" "CANCELLED(c@/)"))))
    ;;Avoid setting entries as DONE when there are still sub-entries that are not DONE.
    (setopt org-enforce-todo-dependencies t)

    ;; Assumes org-clock-taskbar is installed to always visible menubar?
    ;; (add-hook 'org-clock-in-hook (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e" (concat "tell application \"org-clock-statusbar\" to clock in \"" (replace-regexp-in-string "\"" "\\\\\"" org-clock-current-task) "\""))))
    ;; (add-hook 'org-clock-out-hook (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e" "tell application \"org-clock-statusbar\" to clock out")))

    ;; Agenda
    (setopt org-agenda-files '("~/org/learning.org" "~/org/life.org"))
    (setopt org-agenda-skip-scheduled-if-done t)
    (setopt org-agenda-skip-deadline-if-done t)
    (setopt org-agenda-custom-commands
            '(("z" "Super zaen view"
               ((agenda "" ((org-agenda-span 'day)
                            (org-super-agenda-groups
                             '((:name "Schedule"
                                      :time-grid t)
                               (:name "Today"
                                      :scheduled today
                                      :deadline today)
                               (:name "Overdue"
                                      :deadline past
                                      :scheduled past)))))
                (alltodo "" ((org-agenda-overriding-header "")
                             (org-super-agenda-groups
                              '((:name "Inbox"
                                       :category "inbox"
                                       :order 3)
                                (:name "In Progress"
                                       :todo "IN_PROGRESS"
                                       :order 1)
                                (:name "Due Today"
                                       :deadline today
                                       :order 2)
                                (:discard (:category "recurring"))
                                (:name "Important"
                                       :tag "Important"
                                       :priority "A"
                                       :order 6)
                                (:name "Due Soon"
                                       :deadline future
                                       :order 8)
                                (:name "trivial"
                                       :priority<= "C"
                                       :tag ("Trivial" "Unimportant")
                                       :todo ("SOMEDAY")
                                       :order 90)))))))))
    (setopt org-persp-startup-with-agenda "z")

    ;; Journal
    (setopt org-journal-dir "~/org/journal/")
    (setopt org-journal-file-format "%Y%m%d")
    (setopt org-journal-file-type 'weekly)
    (setopt org-journal-start-on-weekday 1)
    (setopt org-journal-encrypt-journal t)

    ;; Noter
    (setq org-noter-default-notes-file-names '("~/org/learning.org")
          org-noter-notes-search-path '("~/org"))

    ;; Time management and recording
    (setopt org-deadline-warning-days 5)
    (setopt org-clock-into-drawer t)
    (setopt org-clock-persist t)
    ;; (setopt org-clocktable-defaults '(:scope ("~/org/work.org" "~/org/work.org_archive") :maxlevel 2 :narrow 200! :block today))
    (setopt spaceline-org-clock-p t) ;; Mode line display of task
    (setopt org-columns-default-format "%60ITEM(Task) %20TODO %10Effort(Effort){:} %10CLOCKSUM")
    (setopt org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                          ("STYLE_ALL" . "habit"))))
    (setopt org-duration-format '((special . h:mm)))
    ;; This has been replaced by org-duration-format above: (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
    ;; (setq org-icalendar-timezone "Europe/London") ;; for ox-icalendar.el if nil uses (current-time-zone) output

    ;; Babel
    ;; (setq org-confirm-babel-evaluate '(not (y-or-n-p "evaluate block? ")))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((clojure . t)
       (dot . t)
       (emacs-lisp . t)
       (gptel . t)
       (js . t)
       (python . t)
       (shell . t)
       (sqlite . t)
       ))
    ) ;; end with-eval-after-load

  ;; tree-sitter settings
  (setopt treesit-language-source-alist
          '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
            (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.2")
            (clojure "https://github.com/sogaiu/tree-sitter-clojure" "v0.0.13")
            (elisp "https://github.com/Wilfred/tree-sitter-elisp" "1.5.0")
            (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
            (html "https://github.com/tree-sitter/tree-sitter-html" "v0.23.2")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
            (json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
            (make "https://github.com/alemuller/tree-sitter-make")
            (markdown "https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1")
            (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
            (toml "https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2")
            (yaml "https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0")))

  ;;
  ;; (spacemacs|define-custom-layout "@K9s"
  ;;   :binding "K"
  ;;   :body
  ;;   (progn
  ;;     ;; hook to add all ERC buffers to the layout
  ;;     (defun spacemacs-layouts/add-k9s-buffer-to-persp ()
  ;;       (persp-add-buffer (current-buffer)
  ;;                         (persp-get-by-name
  ;;                          k9s-spacemacs-layout-name)))
  ;;     (add-hook 'vterm-mode-hook #'spacemacs-layouts/add-k9s-buffer-to-persp)
  ;;     ;; Start vterm maximized and running k9s
  ;;     (call-interactively 'erc)))

  ;; Projectile settings
  ;; See: https://github.com/syl20bnr/spacemacs/issues/4207 should improve speed
  ;; of helm-projectile by using a shell that doesn't have a lot of profile information
  ;; Previously tried
  ;; (setq shell-file-name "/bin/sh")
  (setopt projectile-project-search-path '(("~/proj/" . 2)))
  ;; (setq projectile-create-missing-test-files t)
  ;; (setq projectile-enable-caching t)
  ;; (setq projectile-indexing-method 'native)
  ;; OPTIONAL configuration

  ;; LLM GPTel setup
  ;; See options at: https://github.com/karthink/gptel?tab=readme-ov-file#ollama
  ;; TODO: Evaluate switching this to LM Studio to make use of MLX
  (with-eval-after-load 'gptel
    (gptel-make-kagi "Kagi" :key (auth-source-pick-first-password :host "api.kagi.com"))
    (setopt
     gptel-model 'qwen/qwen3-14b
     gptel-backend (gptel-make-openai "LMStudio"
                     :host "localhost:1234"
                     :protocol "http"
                     :endpoint "/v1/chat/completions"
                     :stream t
                     :key "not-needed"
                     :models (s-split "\n"
                                      (shell-command-to-string "curl -s --connect-timeout 0.5 'http://localhost:1234/v1/models' | jq -r '.data[].id' | head -c -1"))))

    ;; Use the system prompt builder function
    (let ((build-directives-fun "~/proj/llm-prompts/gptel-build-directives.el"))
      (when (f-exists-p build-directives-fun)
        (load build-directives-fun)
        (setq gptel-directives (jpt/gptel-build-directives "~/proj/llm-prompts/system-prompts/")
              gptel-system-message (alist-get 'default gptel-directives)))))

  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
