;;; config.el --- personal-devtools layer config file for Spacemacs. -*- lexical-binding: t -*-
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

;; Personal development tooling defaults.

;;; Code:

;; tree-sitter grammar sources not provided by Emacs built-in *-ts-modes
;; Built-in modes define their own sources for: bash, css, dockerfile, html,
;; java, javascript, jsdoc, json, markdown, markdown-inline, toml, tsx,
;; typescript, yaml (and doxygen). Only add grammars Emacs doesn't provide.
(with-eval-after-load 'treesit
  (dolist (source '((elisp "https://github.com/Wilfred/tree-sitter-elisp" "1.5.0")
                    (make "https://github.com/alemuller/tree-sitter-make")
                    (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")))
    (add-to-list 'treesit-language-source-alist source)))

(with-eval-after-load 'projectile
  (setopt projectile-project-search-path '(("~/proj/" . 2))))
;; (setq projectile-create-missing-test-files t)
;; (setq projectile-enable-caching t)
;; (setq projectile-indexing-method 'native)


;;; config.el ends here
