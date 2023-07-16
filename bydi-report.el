;;; bydi-report.el --- Mocking macros -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/bydi
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: extensions

;;; Commentary:
;;
;; Integration of `ert-runner' and `undercover'.

;;; Code:

(require 'bydi (expand-file-name "bydi.el"))

;;; -- Coverage

(defvar bydi-report--text-file "./coverage/results.txt"
  "The file used to store text coverage.")

(defvar bydi-report--json-file "./coverage/.resultset.json"
  "The file used to store the JSON coverage.")

(defun bydi-report--add-up-type (buf type)
  "Add all numbers of TYPE in buffer BUF."
  (let* ((regex (concat type ": \\(?1:[[:digit:]]+\\)"))
         (content (with-current-buffer buf (buffer-string)))
         (numbers (bydi--matches-in-string regex content)))

    (apply '+ (mapcar #'string-to-number numbers))))

(defun bydi-report--calculate-average ()
  "Calculate the average."
  (with-temp-buffer
    (insert-file-contents bydi-report--text-file)

    (when-let* ((relevant (bydi-report--add-up-type (current-buffer) "Relevant"))
                (covered (bydi-report--add-up-type (current-buffer) "Covered")))

      (string-to-number (format "%.2f%%" (* 100 (/ (float covered) relevant)))))))

;;; -- `ert-runner'

(defun bydi-report--record-temp-file (name &rest _)
  "Record temp file NAME."
  (push name bydi--temp-files))

(defun bydi-report--print-temp-files (&rest _)
  "Print created temp files."
  (when bydi--temp-files
    (message
     "\nCreated the following temp files:\n%s"
     bydi--temp-files))

  (advice-remove 'ert-with-temp-file #'bydi-report--record-temp-file))

(defun bydi-setup--ert-runner (reporter)
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

;;; -- `undercover'

(defvar undercover-force-coverage)
(defvar undercover--merge-report)
(declare-function undercover--setup "ext:undercover.el")

(defun bydi-report--setup-undercover (patterns)
  "Set up `undercover' for PATTERNS."
  (when (require 'undercover nil t)
    (message "Setting up `undercover' with %s" patterns)

    (let ((report-format 'text)
          (report-file bydi-report--text-file))

      (setq undercover-force-coverage t)

      (cond
       ((getenv bydi-setup--env-ci)
        (setq report-format 'lcov
              report-file nil))

       ((getenv bydi-setup--env-coverage-with-json)
        (setq undercover--merge-report nil
              report-format 'simplecov
              report-file bydi-report--json-file)))

      (undercover--setup
       (append patterns
               (list
                (list :report-format report-format)
                (list :report-file report-file)
                (list :send-report nil)))))))

;;;###autoload
(defun bydi-ert-runner-setup (&optional reporter)
  "Set up `ert-runner'.

An optional REPORTER function can be passed."
  (bydi-setup--ert-runner reporter))

;;;###autoload
(defun bydi-undercover-setup (patterns)
  "Set up `undercover' for PATTERNS."
  (bydi-report--setup-undercover patterns))

;;;###autoload
(defun bydi-calculate-coverage ()
  "Calculate the coverage using the results file."
  (interactive)

  (if (file-exists-p bydi-report--text-file)
      (let ((average (bydi-report--calculate-average)))

        (message "Combined coverage: %s%%" average))
    (user-error "Text report %s doesn't exist" bydi-report--text-file)))

(provide 'bydi-report)

;;; bydi-report.el ends here
