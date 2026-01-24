;;; funcs.el --- personal-org layer functions file for Spacemacs. -*- lexical-binding: t -*-
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

;;; Commentary:

;; Org helper functions for the personal-org layer.

;;; Code:

(defvar personal-org--babel-loaded nil
  "Track whether org-babel languages have been loaded.")

;;;###autoload
(defun personal-org--enable-ob-gptel-capf ()
  "Enable ob-gptel completion in the current Org buffer."
  (add-hook 'completion-at-point-functions 'ob-gptel-capf nil t))

;;;###autoload
(defun personal-org--org-mode-setup ()
  "Configure per-buffer Org features."
  (when (featurep 'ob-gptel)
    (personal-org--enable-ob-gptel-capf))
  (add-hook 'org-after-tags-change-hook #'jpt/org-crypt-enable-maybe nil t)
  (jpt/org-crypt-enable-maybe))

;;;###autoload
(defun personal-org--ensure-babel-languages (&rest _)
  "Load org-babel languages once, on first execution."
  (unless personal-org--babel-loaded
    (setq personal-org--babel-loaded t)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       ;; (clojure . t)
       (dot . t)
       (emacs-lisp . t)
       (gptel . t)
       (js . t)
       (python . t)
       (shell . t)
       (sqlite . t)
       ))))

(defun jpt/org-crypt-enable-maybe ()
  "Enable org-crypt and disable autosave only in crypt-tagged buffers."
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward ":crypt:" nil t))
    (add-hook 'before-save-hook #'org-encrypt-entries nil t)
    (auto-save-mode -1)))

;;; funcs.el ends here
