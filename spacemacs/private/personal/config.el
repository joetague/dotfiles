;;; config.el --- General config settings            -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Joe Tague

;; Author: Joe Tague <joetague@gmail.com>
;; Keywords:

;; Fixes GPG setup
;; Set the files that are searched for writing tokens
;; by default ~/.authinfo will be used
;; and write a token in unencrypted format
(setopt auth-sources '("~/.authinfo.gpg"))

;; Shim: upstream shell-pop dropped `shell-pop-last-shell-buffer-name', but
;; Spacemacs's shell layer still references it (layers/+tools/shell/funcs.el).
(with-eval-after-load 'shell-pop
  (unless (boundp 'shell-pop-last-shell-buffer-name)
    (defvar shell-pop-last-shell-buffer-name "")))

(with-eval-after-load 'yasnippet
  (defun jpt/yas-recompile-snippet-dir-on-save ()
    "After saving a snippet, recompile its mode directory so JIT loading uses the .yas-compiled-snippets.el file."
    (when (and buffer-file-name (derived-mode-p 'snippet-mode))
      (yas-compile-directory (file-name-directory buffer-file-name))))
  (add-hook 'after-save-hook #'jpt/yas-recompile-snippet-dir-on-save))

(setq vc-follow-symlinks t)

;; Disable auto-save globally; mode-specific hooks (e.g. org-journal) opt out
;; or re-enable as needed.
(setopt auto-save-default nil)

;; eww
;; Rendering settings
(setq shr-max-image-proportion 0.7)
(setq shr-inhibit-images nil)
(setq shr-use-fonts t)
(setq shr-max-width nil)
(setq shr-width 0)
(setq shr-discard-aria-hidden t)
(setq shr-cookie-policy nil)
;;(with-eval-after-load 'shr
;;  (set-face-attribute 'shr nil :family "Terminus (TTF)"))

;; (add-hook 'eww-after-render-hook #'eww-readable)
