;;; packages.el --- personal-org layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Joe Tague <joetague@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defconst personal-org-packages
  '(org-pdftools
    org-ql
    org-super-agenda)
  "The list of Lisp packages required by the personal-org layer.")

(defun personal-org/init-org-pdftools ()
  "Initialize org-pdftools."
  (use-package org-pdftools
    :defer t
    :after org))

(defun personal-org/init-org-ql ()
  "Initialize org-ql."
  (use-package org-ql
    :defer t
    :after org))

(defun personal-org/init-org-super-agenda ()
  "Initialize org-super-agenda."
  (use-package org-super-agenda
    :defer t
    :after org
    :config
    (org-super-agenda-mode)))

;;(ob-gptel :location (recipe
;;          :fetcher github
;;          :repo "jwiegley/ob-gptel"))
;; org-incoming ;; ingest PDF files into your org or org-roam files.
;; org-noter
;; org-sort-tasks ;; sort an unsorted TODO list using mergesort
;; org-mru-clock
;; org-timeblock
;;; packages.el ends here
