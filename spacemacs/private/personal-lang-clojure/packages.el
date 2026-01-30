;;; packages.el --- personal-lang-clojure layer packages file for Spacemacs. -*- lexical-binding: t -*-
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `personal-lang-clojure-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `personal-lang-clojure/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `personal-lang-clojure/pre-init-PACKAGE' and/or
;;   `personal-lang-clojure/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst personal-lang-clojure-packages
  '(clj-deps-new
    jet
    clay)
  "The list of Lisp packages required by the personal-lang-clojure layer."

  ;; (require 'clj-deps-new)
  ;; (require 'jet)
  ;; (require 'clay)

  ;; (clay :location (recipe
  ;;                  :fetcher github
  ;;                  :repo "scicloj/clay.el"))
  ;; clj-deps-new
  ;; clojure-ts-mode
  ;; (neil :location (recipe
  ;;                  :fetcher github
  ;;                  :repo "babashka/neil"))
  ;; quarto-mode

  )
