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

;; tree-sitter settings
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
        (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.2")
        ;; (clojure "https://github.com/sogaiu/tree-sitter-clojure" "v0.0.13")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp" "1.5.0")
        ;; (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
        (html "https://github.com/tree-sitter/tree-sitter-html" "v0.23.2")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
        (json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1")
        (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
        (toml "https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0")))

;; Projectile settings
;; See: https://github.com/syl20bnr/spacemacs/issues/4207 should improve speed
;; of helm-projectile by using a shell that doesn't have a lot of profile information
;; Previously tried
;; (setq shell-file-name "/bin/sh")
(with-eval-after-load 'projectile
  (setopt projectile-project-search-path '(("~/proj/" . 2))))
;; (setq projectile-create-missing-test-files t)
;; (setq projectile-enable-caching t)
;; (setq projectile-indexing-method 'native)

;; Ensure mise environment is used for vterm.
;; (advice-add 'vterm :around #'mise-propagate-env)

;;; config.el ends here
