;;; bydi-report.el --- Mocking macros -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/bydi
;; Version: 0.5.5
;; Package-Requires: ((emacs "29.1"))
;; Keywords: extensions

;;; Commentary:
;;
;; Integration of `ert-runner' and `undercover'.

;;; Code:

(require 'bydi)
(require 'cl-lib)

(defvar bydi-report--temp-files nil)

(defvar bydi-report--env-coverage-with-json "COVERAGE_WITH_JSON"
  "If set, SimpleCov (JSON) format is used.")

(defvar bydi-report--env-ci "CI"
  "Set if in a CI environment.")

(defvar bydi-report--text-file "./coverage/results.txt"
  "The file used to store text coverage.")

(defvar bydi-report--json-file "./coverage/.resultset.json"
  "The file used to store the JSON coverage.")

(defun bydi-report--add-up-type (buf type)
  "Add all numbers of TYPE in buffer BUF."
  (let* ((regex (concat type ": \\(?1:[[:digit:]]+\\)"))
         (content (with-current-buffer buf (buffer-string)))
         (numbers (bydi-report--matches-in-string regex content)))

    (apply '+ (mapcar #'string-to-number numbers))))

(defun bydi-report--matches-in-string (regexp str)
  "Return all matches of REGEXP in STR."
  (let ((matches nil))

    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string 1) matches)))
    matches))

(defun bydi-report--consume-undercover-report ()
  "Consume the report.

This returns the combined relevant and covered lines as well as
an average of the coverage."
  (with-temp-buffer
    (insert-file-contents bydi-report--text-file)

    (when-let* ((relevant (bydi-report--add-up-type (current-buffer) "Relevant"))
                (covered (bydi-report--add-up-type (current-buffer) "Covered")))

      (list
       (string-to-number (format "%.2f%%" (* 100 (/ (float covered) relevant))))
       relevant
       covered
       (- relevant covered)))))

(defun bydi-report--undercover-result ()
  "Print the undercover report.

This includes the average result on top of the full report."
  (and-let* (((file-exists-p bydi-report--text-file))
             (consumed (bydi-report--consume-undercover-report))
             (report (with-temp-buffer
                       (insert-file-contents-literally bydi-report--text-file)
                       (goto-char 0)
                       (delete-line)
                       (buffer-string))))
    (cl-destructuring-bind (average relevant covered missed) consumed
      (message "%s\nAverage : Percent %s%% [Relevant: %s Covered: %s Missed: %s]\n"
               report
               average
               relevant
               covered
               missed))))

(defun bydi-report--record-temp-file (name &rest _)
  "Record temp file NAME."
  (push name bydi-report--temp-files))

(defun bydi-report--print-temp-files (&rest _)
  "Print created temp files."
  (when bydi-report--temp-files
    (message
     "\nCreated the following temp files:\n%s"
     bydi-report--temp-files))

  (advice-remove 'ert-with-temp-file #'bydi-report--record-temp-file))

(defun bydi-report--setup-ert-runner (reporter)
  "Set up `ert-runner'.

An optional REPORTER function can be passed."
  (advice-add 'ert-with-temp-file :before #'bydi-report--record-temp-file)

  (add-hook
   'ert-runner-reporter-run-ended-functions
   #'bydi-report--print-temp-files)

  (when reporter
    (add-hook
     'ert-runner-reporter-run-ended-functions
     reporter)))

(declare-function project-root "project.el")

(defvar bydi-report--test-helper-location "test/test-helper.el")

(defun bydi-report--find-test-helper ()
  "Find the test helper file.

This calls `pop-to-buffer' with flag NO-RECORD."
  (let* ((project (project-current t))
         (root (project-root project))
         (helper (expand-file-name bydi-report--test-helper-location root)))

    (unless (file-exists-p helper)
      (user-error "Project at %s has no test helper!" root))

    (pop-to-buffer (find-file-noselect helper) nil t)))

;;; -- `undercover'

(defvar undercover-force-coverage)
(defvar undercover--merge-report)
(declare-function undercover--setup "ext:undercover.el")

(defun bydi-report--setup-undercover (patterns)
  "Set up `undercover' for PATTERNS.

The text report will be printed to stdout."
  (when (require 'undercover nil t)
    (message "Setting up `undercover' with %s" patterns)

    (let ((report-format 'text)
          (report-file bydi-report--text-file))

      (setq undercover-force-coverage t)

      (cond
       ((getenv bydi-report--env-ci)
        (setq report-format 'lcov
              report-file nil))

       ((getenv bydi-report--env-coverage-with-json)
        (setq undercover--merge-report nil
              report-format 'simplecov
              report-file bydi-report--json-file)))

      (undercover--setup
       (append patterns
               (list
                (list :report-format report-format)
                (list :report-file report-file)
                (list :send-report nil))))

      (when (eq 'text report-format)
        (add-hook 'kill-emacs-hook #'bydi-report--undercover-result 'last)))))

;;; -- API

;;;###autoload
(defun bydi-report-setup-ert-runner (&optional reporter)
  "Set up `ert-runner'.

An optional REPORTER function can be passed."
  (bydi-report--setup-ert-runner reporter))

;;;###autoload
(defun bydi-report-setup-undercover (patterns)
  "Set up `undercover' for PATTERNS."
  (bydi-report--setup-undercover patterns))

;;;###autoload
(defun bydi-report-find-test-helper ()
  "Find the test helper for the current project."
  (interactive)

  (bydi-report--find-test-helper))

;;;###autoload
(cl-defun bydi-report-setup-ert (&key increase-print-depth)
  "Setup `ert'.

If INCREASE-PRINT-DEPTH is t, `ert-batch-print-level' will be
greatly increased. This is especially useful when using
`bydi-match-expansion' if the macro to match is complex."
  (when increase-print-depth
    (setq ert-batch-print-level 10)))

(provide 'bydi-report)

;;; bydi-report.el ends here
