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

(require 'url)

(defun personal-llm//gptel-send-wrapper ()
  "Wrapper function for gptel-send that sets the flag."
  (interactive)
  (call-interactively 'gptel-send)
  (setq personal-llm--gptel-send-called t))

(defun personal-llm//gptel-abort-wrapper ()
  "Wrapper function for gptel-abort that checks if gptel-send has been called."
  (interactive)
  (if personal-llm--gptel-send-called
      (call-interactively 'gptel-abort)))

(defun personal-llm//enable-ob-gptel-capf ()
  "Enable ob-gptel completion in the current Org buffer."
  (add-hook 'completion-at-point-functions #'ob-gptel-capf nil t))

(defun personal-llm//org-mode-setup ()
  "Configure gptel features in Org buffers."
  (when (featurep 'ob-gptel)
    (personal-llm//enable-ob-gptel-capf)))

(defun personal-llm//ensure-ob-gptel (&rest _)
  "Load ob-gptel support before executing Org Babel source blocks."
  (require 'ob-gptel nil t)
  (when (featurep 'ob-gptel)
    (personal-llm//enable-ob-gptel-capf)))

(defgroup personal-llm nil
  "Personal LLM configuration."
  :group 'tools)

(defcustom personal-llm-lmstudio-base-url "http://localhost:1234"
  "Root URL for the LM Studio OpenAI-compatible API."
  :type 'string
  :group 'personal-llm)

(defun personal-llm//lmstudio-normalize-base-url (&optional base-url)
  "Return a normalized LM Studio BASE-URL."
  (let ((url (string-trim (or base-url personal-llm-lmstudio-base-url ""))))
    (when (string-empty-p url)
      (user-error "LM Studio base URL is empty"))
    (replace-regexp-in-string
     "/\\'" ""
     (if (string-match-p "\\`[[:alpha:]][[:alnum:].+-]*://" url)
         url
       (concat "http://" url)))))

(defun personal-llm//lmstudio-models-url (&optional base-url)
  "Return the LM Studio models URL for BASE-URL."
  (concat (personal-llm//lmstudio-normalize-base-url base-url)
          "/v1/models"))

(defun personal-llm//lmstudio-model-ids (json-response)
  "Return model IDs from LM Studio JSON-RESPONSE."
  (let (models)
    (dolist (model (alist-get 'data json-response) (nreverse models))
      (let ((id (alist-get 'id model)))
        (when (stringp id)
          (push id models))))))

(defun personal-llm//fetch-lmstudio-models (&optional base-url)
  "Fetch available LM Studio models from BASE-URL."
  (let ((models-url (personal-llm//lmstudio-models-url base-url))
        (url-request-method "GET")
        (url-request-extra-headers '(("Accept" . "application/json"))))
    (if-let* ((buffer (url-retrieve-synchronously models-url t t 2)))
        (unwind-protect
            (with-current-buffer buffer
              (when (and (boundp 'url-http-response-status)
                         (numberp url-http-response-status)
                         (>= url-http-response-status 400))
                (user-error "LM Studio returned HTTP %s" url-http-response-status))
              (goto-char (or (and (boundp 'url-http-end-of-headers)
                                  (symbol-value 'url-http-end-of-headers))
                             (point-min)))
              (personal-llm//lmstudio-model-ids
               (json-parse-buffer :object-type 'alist :array-type 'list)))
          (kill-buffer buffer))
      (user-error "Could not connect to LM Studio at %s" models-url))))

(defun personal-llm/make-lmstudio-backend (models &optional base-url)
  "Create a gptel LM Studio backend with MODELS for BASE-URL."
  (let* ((url (personal-llm//lmstudio-normalize-base-url base-url))
         (parsed-url (url-generic-parse-url url))
         (protocol (url-type parsed-url))
         (host (url-host parsed-url))
         (port (url-port parsed-url)))
    (unless (member protocol '("http" "https"))
      (user-error "LM Studio URL must use http or https: %s" url))
    (unless (and host (not (string-empty-p host)))
      (user-error "LM Studio URL must include a host: %s" url))
    (gptel-make-openai "LMStudio"
      :host (if (and port
                     (not (or (and (string= protocol "http") (= port 80))
                              (and (string= protocol "https") (= port 443)))))
                (format "%s:%d" host port)
              host)
      :protocol protocol
      :endpoint "/v1/chat/completions"
      :stream t
      :key "not-needed"
      :models models)))

(with-eval-after-load 'gptel
  (defun personal-llm/refresh-lmstudio-models (&optional base-url)
    "Fetch available models from LM Studio BASE-URL and update gptel backend.

With a prefix argument, prompt for BASE-URL.  Otherwise use
`personal-llm-lmstudio-base-url'."
    (interactive
     (list (when current-prefix-arg
             (read-string "LM Studio base URL: " personal-llm-lmstudio-base-url))))
    (let ((base-url (or base-url personal-llm-lmstudio-base-url)))
      (message "Fetching models from LM Studio at %s..."
               (personal-llm//lmstudio-normalize-base-url base-url))
      (let ((models (personal-llm//fetch-lmstudio-models base-url)))
        (if (null models)
            (message "No models returned by LM Studio. Is it running?")
          (setq gptel-backend (personal-llm/make-lmstudio-backend models base-url))
          (message "Updated LM Studio models: %s"
                   (mapconcat #'identity models ", ")))))))

(defun personal-llm/claude-notify (title message)
  "Display a macOS notification with TITLE and MESSAGE with sound."
  (let ((escaped-title (replace-regexp-in-string "\"" "\\\\\"" title))
        (escaped-message (replace-regexp-in-string "\"" "\\\\\"" message)))
    (call-process "osascript" nil nil nil
                  "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                               escaped-message escaped-title))))
;;; funcs.el ends here
