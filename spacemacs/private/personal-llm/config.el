;;; config.el --- personal-llm layer config file for Spacemacs. -*- lexical-binding: t -*-
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

;; gptel and LLM integrations.

;;; Code:

(defvar personal-llm--gptel-send-called nil
  "Flag to track if gptel-send has been called at least once.")

(with-eval-after-load 'agent-shell
  ;; Header: 'graphical (icons + styled), 'text (plain), or nil (none)
  (setopt agent-shell-header-style 'text)

  ;; Indicators
  (setopt agent-shell-show-busy-indicator t)
  (setopt agent-shell-busy-indicator-frames '("·" "∘" "○" "∘"))
  (setopt agent-shell-show-context-usage-indicator t)
  (setopt agent-shell-show-usage-at-turn-end t)

  ;; Welcome message & blocks
  (setopt agent-shell-show-welcome-message nil)
  (setopt agent-shell-highlight-blocks t)

  ;; Icons (requires a Nerd Font for graphical display)
  (setopt agent-shell-show-config-icons t)
  (setopt agent-shell-thought-process-icon "💭")
  (setopt agent-shell-permission-icon "🔒")

  ;; Collapse noise by default, keep user messages visible
  (setopt agent-shell-thought-process-expand-by-default nil)
  (setopt agent-shell-tool-use-expand-by-default nil)
  (setopt agent-shell-activity-group-expand-by-default nil)
  (setopt agent-shell-user-message-expand-by-default t))
;;; config.el ends here
