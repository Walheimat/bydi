;;; bydi-ci-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'bydi-ci nil t)

(ert-deftest bydi-ci-setup-paths ()
  (let ((load-path nil)
        (default-directory "/tmp"))

    (bydi ((:ignore getenv))

      (shut-up
        (bydi-ci-setup-paths (list "test" "mock")))

      (should (equal load-path '("/tmp/mock" "/tmp/test" "/tmp"))))))

;;; bydi-ci-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
