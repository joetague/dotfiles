;;; funcs.el --- Functions to make Clojure dev easier  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Joe Tague

;; Author: Joe Tague <joetague@gmail.com>
;; Keywords: languages, tools,

;; def portal to the dev namespace to allow dereferencing via @dev/portal
;; (defun portal.api/open ()
;;   (interactive)
;;   (cider-nrepl-sync-request:eval
;;    "(do (ns dev)
;;        (def portal ((requiring-resolve 'portal.api/open)))
;;        (add-tap (requiring-resolve 'portal.api/submit)))"))

;; (defun portal.api/clear ()
;;   (interactive)
;;   (cider-nrepl-sync-request:eval "(portal.api/clear)"))

;; (defun portal.api/close ()
;;   (interactive)
;;   (cider-nrepl-sync-request:eval "(portal.api/close)"))

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
