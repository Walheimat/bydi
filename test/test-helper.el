;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'undercover nil t)

(let ((source-dir (expand-file-name (or (getenv "GITHUB_WORKSPACE")
                                        default-directory)))
      (report-format 'text)
      (report-file "./coverage/results.txt"))

  (add-to-list 'load-path source-dir)

  (setq undercover-force-coverage t)

  (cond
   ((getenv "CI")
    (setq report-format 'lcov
          report-file nil))

   ((getenv "COVERAGE_WITH_JSON")
    (setq undercover--merge-report nil
          report-format 'simplecov
          report-file "./coverage/.resultset.json")))

  (undercover--setup
   (append (list "bydi.el" "bydi-report.el" "bydi-ci.el")
           (list
            (list :report-format report-format)
            (list :report-file report-file)
            (list :send-report nil)))))

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
