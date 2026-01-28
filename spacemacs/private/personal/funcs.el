;;; funcs.el --- personal layer functions file for Spacemacs. -*- lexical-binding: t -*-
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

;; Custom utility functions for personal workflow

;;; Code:

(require 'subr-x)

(defun jpt/delete-to-bol (delete-newline)
  "Delete current line and preceding newline char if DELETE-NEWLINE is set."
  (interactive "p")
  (delete-region (pos-bol) (pos-eol))
  (when delete-newline
    (delete-char -1)))

(defun jpt/move-line-up ()
  "Move current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun jpt/move-line-down ()
  "Move current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun jpt/duplicate-line ()
  "Duplicate current line and append under the current one."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(defun jpt/decode-jwt ()
  "Decode JWT that is on the current line."
  (interactive)
  (let* ((data (split-string (thing-at-point 'filename) "\\."))
         (header (car data))
         (claims (cadr data)))
    (with-temp-buffer
      (insert (format "%s\n\n%s"
                      (base64-decode-string header t)
                      (base64-decode-string claims t)))
      (json-pretty-print-buffer)
      (with-output-to-temp-buffer "*JWT*"
        (special-mode)
        (princ (buffer-string))))) t)

(defun jpt/generate-uuid ()
  "Insert a generated UUID at point."
  (interactive)
  (insert (downcase (string-trim (shell-command-to-string "uuidgen")))))

(defun jpt/projectile-kill-other-buffers ()
  "Kill all buffers in current project except for current buffer."
  (interactive)
  (let* ((current-buffer (current-buffer))
         (buffers (projectile-project-buffers)))
    (dolist (buffer buffers)
      (unless (eq buffer current-buffer)
        (kill-buffer buffer)))
    (message "Kill all buffers in project %s except %s"
             (projectile-project-name)
             (buffer-name current-buffer))))

(defun jpt/toggle-true-false ()
  "Toggle the word at point between `true' and `false'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word))
        (case-fold-search nil)) ;; Make search case-sensitive
    (when bounds
      (let* ((start (car bounds))
             (end (cdr bounds))
             (word (buffer-substring-no-properties start end)))
        (cond
         ((string-equal word "true") (delete-region start end) (insert "false"))
         ((string-equal word "false") (delete-region start end) (insert "true")))))))

(defun jpt/unix-timestamp-to-iso8601 ()
  "Convert Unix timestamp at point to an ISO8601 formatted date and display in minibuffer."
  (interactive)
  (let* ((timestamp (thing-at-point 'number t))
         (time (when timestamp (seconds-to-time timestamp))))
    (if time
        (message "ISO 8601 Date: %s" (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))
      (message "No valid Unix timestamp found at point."))))

(defun jpt/claude-notify (title message)
  "Display a macOS notification with TITLE and MESSAGE with sound."
  (call-process "osascript" nil nil nil
                "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                             message title)))

(defun jpt/native-compile-packages ()
  "Native compile all installed packages synchronously."
  (interactive)
  (when (native-comp-available-p)
    (message "Starting native compilation of all packages...")
    (native-compile-async (expand-file-name "elpa" user-emacs-directory) 'recursively t)
    (message "Native compilation queued. Check *Async-native-compile-log* buffer.")))

;;; funcs.el ends here
