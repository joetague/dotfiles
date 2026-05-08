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
