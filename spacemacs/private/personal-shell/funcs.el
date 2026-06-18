;;; funcs.el --- personal-shell layer functions file for Spacemacs. -*- lexical-binding: t -*-

;;; Code:

(defun personal-shell//ghostel-package-directory ()
  "Return the installed Ghostel package directory, if available."
  (when-let* ((library (locate-library "ghostel")))
    (file-name-directory library)))

(defun personal-shell//enable-evil-ghostel ()
  "Enable `evil-ghostel-mode' when the bundled extension is available."
  (when-let* ((ghostel-directory (personal-shell//ghostel-package-directory))
              (extension-directory
               (expand-file-name "extensions/evil-ghostel" ghostel-directory)))
    (when (file-directory-p extension-directory)
      (add-to-list 'load-path extension-directory)))
  (when (require 'evil-ghostel nil t)
    (add-hook 'ghostel-mode-hook #'evil-ghostel-mode)))

;;; funcs.el ends here
