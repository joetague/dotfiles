;;; config.el --- personal-shell layer configuration file for Spacemacs. -*- lexical-binding: t -*-

;;; Code:

;; Variables
(defvar shell-default-shell (if (spacemacs/system-is-mswindows)
                                'eshell
                              'ghostel)
  "Default shell to use in Spacemacs.

Possible values are `ansi-term' (default for Linux/macOS),
`eshell' (default for windows), `shell', `term', `eat', `vterm',
`multi-term' and `multi-vterm'.")

(spacemacs|defc shell-default-position 'bottom
  "Position of the shell. Possible values are `top', `bottom', `full',
  `left' and `right'."
  '(choice (const top) (const bottom) (const full) (const left) (const right)))

(spacemacs|defc shell-default-height 30
  "Height in percents for the shell window."
  'integer)

(defvar shell-default-width 30
  "Width in percents for the shell window.")

(defvar shell-default-term-shell shell-file-name
  "Default shell to use in `ghostel' shells.")

(defvar shell-default-full-span t
  "If non-nil, the `shell' buffer spans full width of a frame.")

(define-obsolete-variable-alias
  'close-window-with-terminal
  'shell-close-window-with-terminal
  "2025-02-17")

(defvar shell-enable-ghostel-support
  (and module-file-suffix (not (spacemacs/system-is-mswindows)))
  "If non-nil, enable the `ghostel' package.

Requires libghostty to be installed on the system.")

;;; config.el ends here
