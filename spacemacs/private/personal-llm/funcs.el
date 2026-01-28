;;; funcs.el --- personal-llm layer functions file for Spacemacs. -*- lexical-binding: t -*-
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

;; LLM helper functions for the personal-llm layer.

;;; Code:

(require 'subr-x)

(with-eval-after-load 'gptel
  (defun jpt/refresh-lmstudio-models ()
    "Fetch available models from LMStudio and update gptel backend."
    (interactive)
    (message "Fetching models from LMStudio...")
    (let ((models-output (shell-command-to-string
                          "curl -s --connect-timeout 2 'http://localhost:1234/v1/models' | jq -r '.data[].id' 2>/dev/null")))
      (if (string-empty-p (string-trim models-output))
          (message "Could not fetch models from LMStudio. Is it running?")
        (let ((models (split-string (string-trim models-output) "\n" t)))
          (setq gptel-backend (gptel-make-openai "LMStudio"
                                :host "localhost:1234"
                                :protocol "http"
                                :endpoint "/v1/chat/completions"
                                :stream t
                                :key "not-needed"
                                :models models))
          (message "Updated LMStudio models: %s" (string-join models ", ")))))))

(defun jpt/claude-notify (title message)
  "Display a macOS notification with sound."
  (call-process "osascript" nil nil nil
                "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                             message title)))
;;; funcs.el ends here
