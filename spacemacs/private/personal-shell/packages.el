;;; packages.el --- personal-shell layer packages file for Spacemacs. -*- lexical-binding: t -*-

;; Copyright (c) 2012-2026 Sylvain Benner & Contributors
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

;; The `shell' layer owns `ghostel' and `evil-ghostel' as of upstream
;; a10be2cb1, so this layer defines no `init-' functions: ownership is
;; `(fboundp personal-shell/init-<pkg>)', and declaring one here would
;; shadow the upstream setup instead of extending it.  No `:toggle'
;; either -- the last declared toggle wins, which would override
;; `shell-enable-ghostel-support'.
(defconst personal-shell-packages
  '(ghostel
    hungry-delete)
  "The list of Lisp packages required by the personal-shell layer.")

(defun personal-shell/post-init-ghostel ()
  "Add what the `shell' layer leaves out of its Ghostel setup."
  ;; `spacemacs/projectile-shell' (`SPC p $') dispatches on
  ;; `projectile-run-<shell-default-shell>' and silently falls back to
  ;; `projectile-run-shell' when that symbol is unbound.
  (defalias 'projectile-run-ghostel #'ghostel-project)
  ;; `ghostel-other' is the one command with no autoload cookie.
  (autoload 'ghostel-other "ghostel" nil t)
  ;; Upstream binds no major mode keys for `ghostel-mode'.
  (spacemacs/set-leader-keys-for-major-mode 'ghostel-mode
    "c" #'ghostel
    "l" #'ghostel-list-buffers
    "n" #'ghostel-next
    "N" #'ghostel-previous
    "p" #'ghostel-previous
    "r" #'rename-buffer)
  ;; Deferred to keep these opt-in integrations from pulling in Ghostel
  ;; at startup, matching the `:config' placement they had previously.
  (with-eval-after-load 'ghostel
    (when personal-shell-enable-ghostel-compile-global-mode
      (require 'ghostel-compile)
      (ghostel-compile-global-mode 1))
    (when personal-shell-enable-ghostel-eshell-visual-command-mode
      (require 'ghostel-eshell)
      (add-hook 'eshell-load-hook #'ghostel-eshell-visual-command-mode))))

(defun personal-shell/post-init-hungry-delete ()
  "Keep hungry-delete out of Ghostel terminal buffers.
The `spacemacs-editing' layer excludes only `term-mode' and `vterm-mode'."
  (with-eval-after-load 'hungry-delete
    (add-to-list 'hungry-delete-except-modes 'ghostel-mode)))

;;; packages.el ends here
