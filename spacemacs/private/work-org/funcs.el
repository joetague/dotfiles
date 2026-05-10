;;; funcs.el --- work-org layer functions file for Spacemacs. -*- lexical-binding: t -*-
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

;; Org helper functions for the work-org layer.

;;; Code:

(require 'seq)

(defun work-org--feedback-template (member)
  "Build a feedback capture template for MEMBER (a plist)."
  (let ((key (plist-get member :key))
        (name (plist-get member :name))
        (group (plist-get member :group)))
    (list
     (concat "f" key)
     name
     'entry
     (list 'file+olp work-org-feedback-file "Feedback" group name)
     work-org-feedback-template
     :work-org t)))

(defun work-org--feedback-others-template ()
  "Catch-all `Others' feedback template."
  (list "f~" "Others" 'entry
        (list 'file+olp work-org-feedback-file "Feedback" "Others")
        work-org-feedback-template
        :work-org t))

(defun work-org--feedback-templates ()
  "Return org-capture templates for work feedback."
  (append
   (list (list "f" "Feedback"))
   (mapcar #'work-org--feedback-template work-org-team-members)
   (list (work-org--feedback-others-template))))

(defun work-org--work-org-template-p (template)
  "Return non-nil when TEMPLATE belongs to the work-org layer."
  (let ((plist (nthcdr 5 template)))
    (plist-get plist :work-org)))

(defun work-org--install-feedback-templates ()
  "Install feedback capture templates, replacing any previous work-org ones."
  (setq org-capture-templates
        (append
         (seq-remove
          (lambda (template)
            (or (work-org--work-org-template-p template)
                (and (consp template)
                     (stringp (car-safe template))
                     (stringp (car-safe (cdr-safe template)))
                     (string= (car template) "f")
                     (string= (cadr template) "Feedback")
                     (null (nth 2 template)))))
          org-capture-templates)
         (work-org--feedback-templates))))

;;; funcs.el ends here
