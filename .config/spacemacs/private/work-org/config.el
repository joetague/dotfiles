;;; config.el --- work-org layer config file for Spacemacs. -*- lexical-binding: t -*-
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

;; Work Org mode configuration.

;;; Code:

(defvar work-org-team-members
  '("Team Member 1"
    "Team Member 2")
  "List of team members used to generate feedback capture templates.")

(defvar work-org-feedback-file
  "~/org/work/feedback.org"
  "Org file where work feedback entries are stored.")

(with-eval-after-load 'org
  (work-org--install-feedback-templates)
  (setq org-clock-clocktable-default-properties '(:scope (work-org-feedback-file (concat work-org-feedback-file "_archive")) :maxlevel 2 :narrow 200! :block today))
  )

;;; config.el ends here
