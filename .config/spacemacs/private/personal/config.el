;;; config.el --- General config settings            -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Joe Tague

;; Author: Joe Tague <joetague@gmail.com>
;; Keywords:

;; Fixes GPG setup
;; Set the files that are searched for writing tokens
;; by default ~/.authinfo will be used
;; and write a token in unencrypted format
(setopt auth-sources '("~/.authinfo.gpg"))
