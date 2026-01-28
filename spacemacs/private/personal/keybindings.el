;;; keybindings.el --- personal layer keybindings file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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

;; Personal keybindings using SPC o prefix (user-reserved)

;;; Code:

;; Define prefix categories
(spacemacs/declare-prefix "o" "personal")
(spacemacs/declare-prefix "ot" "text manipulation")
(spacemacs/declare-prefix "ou" "utilities")
(spacemacs/declare-prefix "op" "projectile")

;; Bind personal functions
(spacemacs/set-leader-keys
  "otl" 'jpt/delete-to-bol
  "otu" 'jpt/move-line-up
  "otd" 'jpt/move-line-down
  "otD" 'jpt/duplicate-line
  "ott" 'jpt/toggle-true-false
  "opk" 'jpt/projectile-kill-other-buffers
  "ouj" 'jpt/decode-jwt
  "oug" 'jpt/generate-uuid
  "out" 'jpt/unix-timestamp-to-iso8601
  "ouc" 'jpt/native-compile-packages)

;;; keybindings.el ends here
