;;; config.el --- personal-shell layer configuration file for Spacemacs. -*- lexical-binding: t -*-

;;; Code:

;; `evil-ghostel' is owned and enabled by the `shell' layer, gated on
;; `shell-enable-ghostel-support'.

(defvar personal-shell-enable-ghostel-compile-global-mode nil
  "If non-nil, make `compile' and related commands use Ghostel.")

(defvar personal-shell-enable-ghostel-eshell-visual-command-mode nil
  "If non-nil, run Eshell visual commands in Ghostel buffers.")

;;; config.el ends here
