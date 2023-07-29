;;; bydi-report-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'bydi-report)

(defvar coverage-file (ert-resource-file "coverage.txt"))
(defvar mock-coverage-file (ert-resource-file "mock-coverage.txt"))

(ert-deftest bydi-report--matches-in-string ()
  (let ((str "This 1 string has 3 matches, or is it 2?")
        (pattern "\\(?1:[[:digit:]]\\)"))

    (should (equal '("2" "3" "1") (bydi-report--matches-in-string pattern str)))))

(ert-deftest bydi-report--report ()
  (bydi (message)
    (let ((bydi--temp-files nil))
      (bydi-report--print-temp-files)
      (bydi-was-not-called message)

      (bydi-report--record-temp-file "test")
      (bydi-report--print-temp-files)
      (bydi-was-called-with message (list "\nCreated the following temp files:\n%s"
                                          '("test"))))))

;; This test will actually print out the coverage for `bydi', which is
;; a nice side effect.
(ert-deftest bydi-undercover-setup ()
  (bydi (undercover--setup
         (:mock getenv :with ignore))

    (bydi-report-setup-undercover (list "bydi.el"))

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

(ert-deftest bydi-report--undercover-result ()
  (let ((bydi-report--text-file mock-coverage-file))

    (shut-up
      (ert-with-message-capture messages

        (bydi ((:mock bydi-report--consume-undercover-report :return '(10 3 2 1)))
          (bydi-report--undercover-result))

        (should (string=
                 "COVERAGE\n\nAverage : Percent 10% [Relevant: 3 Covered: 2 Missed: 1]\n\n"
                 messages))))))

(ert-deftest bydi-report-setup-ert-runner ()
  (bydi (add-hook)

    (bydi-ert-runner-setup 'always) ;; To be removed.
    (bydi-report-setup-ert-runner 'always)

    (bydi-was-called add-hook)
    (bydi-was-called-n-times add-hook 4)
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
