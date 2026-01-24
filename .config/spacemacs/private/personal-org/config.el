;;; config.el --- personal-org layer config file for Spacemacs. -*- lexical-binding: t -*-
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

;; Personal Org mode configuration.

;;; Code:

(with-eval-after-load 'org-journal
  (setopt
   org-journal-dir "~/org/journal/"
   org-journal-file-format "%Y%m%d"
   org-journal-file-type 'weekly
   org-journal-start-on-weekday 1
   org-journal-encrypt-journal t))

(with-eval-after-load 'org-noter
  (setopt
   org-noter-default-notes-file-names '("~/org/learning.org")
   org-noter-notes-search-path '("~/org")))

(with-eval-after-load 'org-persp
  (setopt org-persp-startup-with-agenda "z"))

(with-eval-after-load 'org
  (require 'org-crypt)

  (advice-add 'org-babel-execute-src-block :before #'personal-org--ensure-babel-languages)
  (with-eval-after-load 'ob-gptel
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-mode)
          (personal-org--enable-ob-gptel-capf)))))

  (add-hook 'org-mode-hook #'personal-org--org-mode-setup)

  ;; GPG key to use for encryption. Set to nil to use symmetric encryption.
  (setopt
   ;; Agenda
   org-agenda-files '("~/org/learning.org" "~/org/life.org")
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-custom-commands
   '(("z" "Super zaen view"
      ((agenda "" ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '((:name "Schedule"
                             :time-grid t)
                      (:name "Today"
                             :scheduled today
                             :deadline today)
                      (:name "Overdue"
                             :deadline past
                             :scheduled past)))))
       (alltodo "" ((org-agenda-overriding-header "")
                    (org-super-agenda-groups
                     '((:name "Inbox"
                              :category "inbox"
                              :order 3)
                       (:name "In Progress"
                              :todo "IN_PROGRESS"
                              :order 1)
                       (:name "Due Today"
                              :deadline today
                              :order 2)
                       (:discard (:category "recurring"))
                       (:name "Important"
                              :tag "Important"
                              :priority "A"
                              :order 6)
                       (:name "Due Soon"
                              :deadline future
                              :order 8)
                       (:name "trivial"
                              :priority<= "C"
                              :tag ("Trivial" "Unimportant")
                              :todo ("SOMEDAY")
                              :order 90))))))))

   ;; Encryption
   org-tags-exclude-from-inheritance (quote ("crypt"))
   org-crypt-key "F4E72EEA776B3FBC"

   ;; Capturing
   org-refile-targets '((nil :maxlevel . 4)
                        (org-agenda-files :maxlevel . 4))
   org-outline-path-complete-in-steps nil         ; Refile in a single go
   org-refile-use-outline-path t                  ; Show full paths for refiling

   ;; TODO Keywords
   org-todo-keywords
   (quote ((sequence "TODO(t)" "IN_PROGRESS(i)" "|" "DONE(d)")
           (sequence "WAITING(w@/)" "HOLD(h@/)" "|" "CANCELLED(c@/)")))
   ;; Avoid setting entries as DONE when there are still sub-entries that are not DONE.
   org-enforce-todo-dependencies t

   ;; Time management and recording
   org-deadline-warning-days 5
   org-clock-into-drawer t
   org-clock-persist t
   spaceline-org-clock-p t ;; Mode line display of task
   org-columns-default-format "%60ITEM(Task) %20TODO %10Effort(Effort){:} %10CLOCKSUM"
   org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                 ("STYLE_ALL" . "habit")))
   org-duration-format '((special . h:mm)))

  ;; https://emacs.stackexchange.com/questions/12900/passing-a-variable-to-template-function-in-org-capture-templates
  ;; Creating a template for meeting notes
  (defvar my/org-meeting-template "** %<%Y-%m-%d %H:%M> %^{something} %(org-set-tags \"crypt\")
    SCHEDULED: %<%Y-%m-%d %H:%M>
    *Attendees:*
    - [X] Joe Tague
    - [ ] %?
    *Agenda:*
    -
    -
    *Notes:*
    " "Meeting Template")

  ;; Babel languages load lazily on first src block execution.
  )

;;; config.el ends here
