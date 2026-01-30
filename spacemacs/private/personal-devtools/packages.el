;;; packages.el --- personal-devtools layer packages file for Spacemacs. -*- lexical-binding: t -*-
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

(defconst personal-devtools-packages
  '(mise)
  "The list of Lisp packages required by the personal-devtools layer.")

(defun personal-devtools/init-mise ()
  "Initialize mise."
  (use-package mise
    :defer t
    :init
    (add-hook 'after-init-hook #'global-mise-mode)
    :config
    ;; Ensure mise environment is used for vterm.
    (advice-add 'vterm :around #'mise-propagate-env)))

;;; packages.el ends here
