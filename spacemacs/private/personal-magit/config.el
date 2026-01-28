;;; config.el --- personal-magit layer config file for Spacemacs. -*- lexical-binding: t -*-
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

;; Magit and Forge preferences.

;;; Code:

(with-eval-after-load 'magit
  (setq magit-git-executable "/opt/homebrew/bin/git")
  (setq magit-process-connection-type nil)
  (setopt magit-diff-refine-hunk 'all)
  (setopt magit-repository-directories
          '(("~/.emacs.d"  . 0)
            ("~/proj/" . 4)))
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-worktrees
                          'magit-insert-status-headers t))

(with-eval-after-load 'forge
  (setopt forge-topic-list-limit '(100 . -5))
  (setopt forge-owned-accounts
          '(("joetague"))))

;;; config.el ends here
