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
     ;; (aider)

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

     ;; (claude-code :variables
     ;;              claude-code-ide-window-side 'right
     ;;              claude-code-ide-window-width 100)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/clojure/README.org
     ;; (clojure :variables
     ;;          clojure-backend 'cider
     ;;          clojure-enable-linters '(clj-kondo joker)
     ;;          clojure-enable-clj-refactor t
     ;;          clojure-enable-kaocha-runner t
     ;;          clojure-toplevel-inside-comment-form t)

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
                 js2-mode-show-parse-errors nil
                 node-add-modules-path t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/json/README.org
     (json :variables
           json-backend 'lsp
           json-fmt-on-save nil)

     ;; Large Language Model
     ;; This is mainly for gptel - split into own layer?
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
     markdown

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

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/protobuf/README.org
     ;; protobuf

     ;; Just use IntelliJ with plugins?
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/python/README.org
     (python :variables
             python-auto-set-local-pyenv-version 'on-project-switch
             python-auto-set-local-pyvenv-virtualenv 'on-project-switch
             python-backend 'lsp
             python-lsp-server 'pyright
             python-test-runner 'pytest)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/restclient/README.org
     restclient

     ;; Just use VSCode?
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+frameworks/react/README.org
     ;; react

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+tools/shell/README.org
     (shell :variables
            shell-default-shell 'vterm
            shell-default-height 30
            shell-default-position 'bottom
            spacemacs-vterm-history-file-location "~/.zsh_history")

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/shell-scripts/README.org
     (shell-scripts :variables
                    shell-scripts-backend 'lsp)

     ;; spacemacs-layouts layer added to set variables
     ;; SPC TAB restricted to current layout buffers
     ;; Kill buffers when killing layer - SPC l x
     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+spacemacs/spacemacs-layouts/README.org
     (spacemacs-layouts :variables
                        spacemacs-layouts-restrict-spc-tab t
                        persp-autokill-buffer-on-remove t)

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
     (typescript :variables
                 typescript-backend 'lsp
                 typescript-lsp-linter nil)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+source-control/version-control/README.org
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)

     ;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/yaml/README.org
     (yaml :variables
           yaml-enable-lsp t)

     ;; Custom personal layer
     personal
     personal-devtools
     personal-lsp
     personal-magit
     personal-org
     personal-llm

     )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '()

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
   dotspacemacs-gc-cons '(200000000 0.1)  ; 200MB for Emacs 31

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 3 1024 1024)  ; 3MB for LSP

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
   dotspacemacs-default-icons-font 'nerd-icons

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
   dotspacemacs-large-file-size 5

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

   ;; Make consecutive tab key presses after commands such as
   ;; `spacemacs/alternate-buffer' (SPC TAB) cycle through previous
   ;; buffers/windows/etc. Please see the option's docstring for more information.
   ;; Set the option to t in order to enable cycling for all current and
   ;; future cycling commands. Alternatively, choose a subset of the currently
   ;; supported commands: '(alternate-buffer alternate-window). (default nil)
   dotspacemacs-enable-cycling nil

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
   dotspacemacs-enable-package-quickstart t

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
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers '(
                               :relative nil
                               :visual t
                               :disabled-for-modes dired-mode
                               doc-view-mode
                               markdown-mode
                               org-mode
                               pdf-view-mode
                               text-mode
                               :size-limit-kb 1000)

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
   ;; `undo-redo', `undo-fu' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system from or to undo-tree. (default `undo-redo')
   dotspacemacs-undo-system 'undo-redo

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
   dotspacemacs-whitespace-cleanup 'changed  ; Only changed lines

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
   dotspacemacs-byte-compile t))

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
  ;; Stable packages configuration
  (add-to-list 'configuration-layer-elpa-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

  ;; Keep magit ecosystem stable
  (add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(magit-section . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(transient . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(forge . "melpa-stable") t)

  ;; Keep GPTel stable (extensive custom config)
  (add-to-list 'package-pinned-packages '(gptel . "melpa-stable") t)

  ;; Native compilation settings (Emacs 28+)
  ;; Disable async compilation - compile immediately instead
  (when (native-comp-available-p)
    (setq native-comp-deferred-compilation nil)  ; No async, compile synchronously
    (setq native-comp-speed 2)                   ; Optimization level (0-3, default 2)
    (setq native-comp-async-jobs-number 4)       ; Parallel compilation jobs
    (setq native-comp-async-report-warnings-errors nil))  ; Suppress async warnings

  (advice-add
   'company-statistics--load :around
   (lambda (orig-fun)
     (let ((warning-inhibit-types '((files missing-lexbind-cookie))))
       (funcall orig-fun))))
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  ;; This may disappear in v31+?
  (setopt spacemacs-keep-legacy-current-buffer-delete-bindings nil)

  ;; Always load newest byte code
  (setopt load-prefer-newer t)

  ;; Remove window decoration
  (add-to-list 'default-frame-alist '(undecorated-round . t))

  ;; Emacs text rendering optimisations
  ;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
  ;; Only render text left to right
  (setopt bidi-paragraph-direction 'left-to-right)

  ;; Per-machine local overrides (not tracked)
  (let ((local (expand-file-name "~/.spacemacs.local")))
    (when (file-readable-p local)
      (load-file local)))
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
   '(package-selected-packages
     '(ace-jump-helm-line ace-link add-node-modules-path aggressive-indent
                          all-the-icons auto-compile auto-highlight-symbol
                          auto-yasnippet blacken browse-at-remote
                          centered-cursor-mode clean-aindent-mode clj-deps-new
                          code-cells column-enforce-mode company-anaconda
                          company-emoji company-quickhelp company-restclient
                          company-shell company-statistics company-web
                          copy-as-format counsel counsel-gtags cython-mode
                          dash-at-point devdocs diff-hl diminish dired-quick-sort
                          disable-mouse dotenv-mode drag-stuff dumb-jump eat
                          editorconfig elfeed-goodies elfeed-org elisp-def
                          elisp-demos elisp-slime-nav ellama emmet-mode
                          emoji-cheat-sheet-plus emr esh-help eshell-prompt-extras
                          eshell-z eval-sexp-fu evil-anzu evil-args
                          evil-cleverparens evil-collection evil-easymotion
                          evil-escape evil-evilified-state evil-exchange
                          evil-goggles evil-iedit-state evil-indent-plus evil-lion
                          evil-lisp-state evil-matchit evil-mc evil-nerd-commenter
                          evil-numbers evil-org evil-surround evil-textobj-line
                          evil-tutor evil-unimpaired evil-visual-mark-mode
                          evil-visualstar expand-region eyebrowse fancy-battery
                          fish-mode flx-ido flycheck-bashate flycheck-elsa
                          flycheck-package flycheck-pos-tip ggtags gh-md gh-notify
                          git-link git-messenger git-modes git-timemachine
                          gitignore-templates gnuplot golden-ratio
                          google-translate gptel grizzl groovy-imports groovy-mode
                          helm-ag helm-c-yasnippet helm-comint helm-company
                          helm-css-scss helm-dash helm-descbinds helm-git-grep
                          helm-ls-git helm-lsp helm-make helm-mode-manager
                          helm-org helm-org-rifle helm-projectile helm-purpose
                          helm-pydoc helm-swoop helm-themes helm-xref hide-comnt
                          highlight-indentation highlight-numbers
                          highlight-parentheses hl-todo holy-mode hungry-delete
                          hybrid-mode impatient-mode import-js importmagic
                          indent-guide info+ insert-shebang inspector ivy jet
                          js-doc js2-mode js2-refactor kubernetes kubernetes-evil
                          launchctl live-py-mode livid-mode llm lorem-ipsum
                          macrostep magit-popup markdown-toc maven-test-mode mcp
                          multi-line multi-term multi-vterm multiple-cursors mvn
                          nameless neil nerd-icons nerd-icons-completion
                          nodejs-repl nose npm-mode ob-http ob-restclient
                          open-junk-file org-cliplink org-contrib org-download
                          org-journal org-mime org-noter org-pdftools org-pomodoro
                          org-present org-projectile org-rich-yank org-superstar
                          orgit-forge osx-clipboard osx-dictionary osx-trash
                          overseer ox-gfm paradox password-generator pcache
                          pcre2el pdf-view-restore pip-requirements pipenv pippel
                          plz poetry poly-markdown polymode prettier-js pug-mode
                          py-isort pydoc pyenv-mode pylookup pytest quarto-mode
                          quickrun rainbow-delimiters restart-emacs
                          restclient-helm reveal-in-osx-finder sass-mode scss-mode
                          shell-pop shfmt skewer-mode slim-mode smeargle space-doc
                          spaceline spacemacs-purpose-popwin
                          spacemacs-whitespace-cleanup sphinx-doc sql-indent
                          sqlup-mode string-edit-at-point string-inflection swiper
                          symbol-overlay symon tagedit term-cursor terminal-here
                          tern toc-org toml-mode treemacs-evil
                          treemacs-icons-dired treemacs-magit treemacs-persp
                          treemacs-projectile undo-fu undo-fu-session valign verb
                          vi-tilde-fringe vim-powerline volatile-highlights vundo
                          web-beautify web-mode which-key winum writeroom-mode
                          ws-butler yaml-mode yapfify yasnippet-snippets))
   '(safe-local-variable-values
     '((eval define-clojure-indent (l/matcha '(1 (:defn))) (l/matche '(1 (:defn)))
             (p.types/def-abstract-type '(1 (:defn)))
             (p.types/defprotocol+ '(1 (:defn)))
             (p.types/defrecord+ '(2 nil nil (:defn)))
             (p.types/deftype+ '(2 nil nil (:defn)))
             (p/def-map-type '(2 nil nil (:defn))) (p/defprotocol+ '(1 (:defn)))
             (p/defrecord+ '(2 nil nil (:defn))) (p/deftype+ '(2 nil nil (:defn)))
             (tools.macro/macrolet '(1 ((:defn)) :form)))
       (eval put 'mu/defn 'clojure-doc-string-elt 2)
       (eval put 'mr/def 'clojure-doc-string-elt 2)
       (eval put 'mi/define-batched-hydration-method 'clojure-doc-string-elt 3)
       (eval put 'mi/define-simple-hydration-method 'clojure-doc-string-elt 3)
       (eval put 'methodical/defmulti 'clojure-doc-string-elt 2)
       (eval put 'methodical/defmethod 'clojure-doc-string-elt 3)
       (eval put 'p.types/defprotocol+ 'clojure-doc-string-elt 2)
       (eval put 's/defn 'clojure-doc-string-elt 2)
       (eval put 'setting/defsetting 'clojure-doc-string-elt 2)
       (eval put 'defsetting 'clojure-doc-string-elt 2)
       (eval put 'api.macros/defendpoint 'clojure-doc-string-elt 3)
       (eval put 'define-premium-feature 'clojure-doc-string-elt 2)
       (whitespace-line-column . 118) (ftf-project-finders ftf-get-top-git-dir)
       (lexical-binding . t) (evil-shift-width . 4) (javascript-backend . tide)
       (javascript-backend . tern) (javascript-backend . lsp))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
