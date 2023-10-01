;;; bydi-ci.el --- CI helpers -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/bydi
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: extensions

;;; Commentary:
;;
;; Utility to integrate with CI.

;;; Code:

(defvar bydi-ci--env-github-workspace "GITHUB_WORKSPACE"
  "Location of the project in GitHub action.")

(defun bydi-ci--setup-paths (paths)
  "Set up `load-path'.

Optionally, set up additional relative PATHS.

This function returns a list of the directories added to the
`load-path'."
  (let* ((source-dir (expand-file-name (or (getenv bydi-ci--env-github-workspace)
                                           default-directory)))
         (paths (append (list source-dir) (mapcar (lambda (it) (expand-file-name it source-dir)) paths))))

    (message "Adding %s to `load-path'" paths)

    (dolist (it paths)
      (add-to-list 'load-path it))

    paths))

;;;###autoload
(defun bydi-ci-setup-paths (&optional paths)
  "Add PATHS to load path in a CI-aware way."
  (bydi-ci--setup-paths paths))

(provide 'bydi-ci)

;;; bydi-ci.el ends here
