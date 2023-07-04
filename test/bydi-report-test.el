;;; bydi-report-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'bydi-report)

(defvar coverage-file (ert-resource-file "coverage.txt"))

(ert-deftest bydi-report--report ()
  (bydi (message)
    (let ((bydi--temp-files nil))
      (bydi-report--print-temp-files)
      (bydi-was-not-called message)

      (setq bydi--temp-files '("/tmp/test"))
      (bydi-report--print-temp-files)
      (bydi-was-called-with message (list "\nCreated the following temp files:\n%s"
                                          '("/tmp/test"))))))

(ert-deftest bydi-undercover-setup ()
  (bydi (undercover--setup
         (:mock getenv :with ignore))

    (bydi-undercover-setup (list "bydi.el"))

    (bydi-was-called-with undercover--setup '(("bydi.el" (:report-format text)
                                               (:report-file "./coverage/results.txt")
                                               (:send-report nil))))))

(ert-deftest bydi-undercover-setup--ci ()
  (bydi (undercover--setup
         (:mock getenv :with (lambda (r) (string= "CI" r))))

    (bydi-undercover-setup (list "bydi.el"))

    (bydi-was-called-with undercover--setup '(("bydi.el" (:report-format lcov)
                                               (:report-file nil)
                                               (:send-report nil))))))

(ert-deftest bydi-undercover-setup--json ()
  (bydi (undercover--setup
         (:mock getenv :with (lambda (r) (string= "COVERAGE_WITH_JSON" r))))

    (bydi-undercover-setup (list "bydi.el"))

    (bydi-was-called-with undercover--setup '(("bydi.el" (:report-format simplecov)
                                               (:report-file "./coverage/.resultset.json")
                                               (:send-report nil))))))


(ert-deftest bydi-ert-runner-setup ()
  (bydi (add-hook)

    (bydi-ert-runner-setup 'always)

    (bydi-was-called add-hook)
    (bydi-was-called-n-times add-hook 2)
    (bydi-was-called-nth-with add-hook '(ert-runner-reporter-run-ended-functions bydi-report--print-temp-files) 0)
    (bydi-was-called-nth-with add-hook '(ert-runner-reporter-run-ended-functions always) 1)))


(ert-deftest bydi-calculate-coverage ()
  (let ((bydi-report--text-file coverage-file))

    (should (string-equal "Combined coverage: 37.33%" (bydi-calculate-coverage)))))

(ert-deftest bydi-calculate-coverage--errors-on-missing-file ()
  (let ((bydi-report--text-file "/tmp/non-existence.txt"))

    (should-error (bydi-calculate-coverage) :type 'user-error)))

;;; bydi-report-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
