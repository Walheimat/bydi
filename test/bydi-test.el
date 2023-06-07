;;; bydi-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'bydi nil t)

(ert-deftest bydi-rf ()
  (should (equal (bydi-rf 'test 'this 'now) 'test)))

(ert-deftest bydi-ra ()
  (should (equal (bydi-ra 'test 'this 'now) '(test this now))))

(ert-deftest bydi-rt ()
  (should (equal (bydi-rt 'test 'this 'now) 'testing)))

(ert-deftest bydi-with-mock--old-usage ()
  (bydi-match-expansion
   (bydi-with-mock (bydi-rf
                    (bydi-rt . #'ignore))
     (should (always)))
   `(cl-letf*
        ((bydi-mock-history
          (make-hash-table :test 'equal))
         (remember
          (lambda (fun args)
            (let* ((prev (gethash fun bydi-mock-history))
                   (val (if prev
                            (push args prev)
                          (list args))))

              (puthash fun val bydi-mock-history)
              args)))
         ((symbol-function 'bydi-rf)
          (lambda (&rest r)
            (interactive)
            (apply remember (list 'bydi-rf r))))
         ((symbol-function 'bydi-rt)
          (lambda (&rest r)
            (interactive)
            (apply remember (list 'bydi-rt r))
            (apply #'ignore r))))
      (should (always)))))

(ert-deftest bydi-with-mock ()
  (bydi-match-expansion
   (bydi-with-mock ((:return "hello" :mock substring)
                    (:mock buffer-file-name :return "/tmp/test.el")
                    (:mock bydi-ra :with ignore)
                    (:with always :mock buffer-live-p))
     (should (always)))
   `(cl-letf*
        ((bydi-mock-history
          (make-hash-table :test 'equal))
         (remember
          (lambda (fun args)
            (let* ((prev (gethash fun bydi-mock-history))
                   (val (if prev
                            (push args prev)
                          (list args))))

              (puthash fun val bydi-mock-history)
              args)))
         ((symbol-function 'substring)
          (lambda (&rest r)
            (interactive)
            (apply remember (list 'substring r))
            "hello"))
         ((symbol-function 'buffer-file-name)
          (lambda (&rest r)
            (interactive)
            (apply remember (list 'buffer-file-name r))
            "/tmp/test.el"))
         ((symbol-function 'bydi-ra)
          (lambda (&rest r)
            (interactive)
            (apply remember (list 'bydi-ra r))
            (apply #'ignore r)))
         ((symbol-function 'buffer-live-p)
          (lambda (&rest r)
            (interactive)
            (apply remember (list 'buffer-live-p r))
            (apply #'always r))))
      (should (always)))))

(ert-deftest bydi-with-mock--single-function ()
  (bydi-match-expansion
   (bydi-with-mock bydi-rf

     (should (always)))
   `(cl-letf*
        ((bydi-mock-history
          (make-hash-table :test 'equal))
         (remember
          (lambda (fun args)
            (let* ((prev (gethash fun bydi-mock-history))
                   (val (if prev
                            (push args prev)
                          (list args))))

              (puthash fun val bydi-mock-history)
              args)))
         ((symbol-function 'bydi-rf)
          (lambda (&rest r)
            (interactive)
            (apply remember
                   (list 'bydi-rf r)))))
      (should (always)))))

(ert-deftest bydi-clear-mocks ()
  (let ((bydi-mock-history nil))
    (bydi-clear-mocks)
    (should bydi-mock-history)))

(ert-deftest bydi-was-called ()
  (bydi-match-expansion
   (bydi-was-called apply)
   '(let ((actual
           (gethash 'apply bydi-mock-history 'not-called)))
      (should-not (equal 'not-called actual)))))

(ert-deftest bydi-was-called-with ()
  (bydi-match-expansion

   (bydi-was-called-with apply '(a b c))
   '(should (equal '(a b c) (car (gethash 'apply bydi-mock-history))))))

(ert-deftest bydi-was-called-with--single-item ()
  (bydi-match-expansion

   (bydi-was-called-with apply "test")
   '(should (equal (list "test") (car (gethash 'apply bydi-mock-history))))))

(ert-deftest bydi-was-called-nth-with ()
  (bydi-match-expansion

   (bydi-was-called-nth-with apply 'test 1)
   '(should (equal 'test (nth 1 (reverse (gethash 'apply bydi-mock-history)))))))

(ert-deftest bydi-was-called-nth-with--single-item ()
  (bydi-match-expansion

   (bydi-was-called-nth-with apply "test" 1)
   '(should (equal (list "test") (nth 1 (reverse (gethash 'apply bydi-mock-history)))))))

(ert-deftest bydi-was-not-called ()
  (bydi-match-expansion
   (bydi-was-not-called apply)
   '(let ((actual
           (gethash 'apply bydi-mock-history 'not-called)))
      (should (equal 'not-called actual)))))

(ert-deftest bydi-was-called-n-times ()
  (bydi-match-expansion

   (bydi-was-called-n-times apply 12)
   '(should (equal 12 (length (gethash 'apply bydi-mock-history))))))

(ert-deftest bydi-match-expansion ()
  (bydi-match-expansion
   (bydi-match-expansion
    (should t))
   '(should (pcase (macroexpand-1 '(should t))))))

(ert-deftest bydi-should-every ()
  (bydi-match-expansion
   (bydi-should-every (a b c) :check 'equal :expected 'test)
   '(progn
      (should ('equal a 'test))
      (should ('equal b 'test))
      (should ('equal c 'test)))))

(ert-deftest bydi-with-temp-file ()
  (bydi-match-expansion
   (bydi-with-temp-file "test"
     (should t))
   '(progn
      (let ((bydi-tmp-file "/tmp/test"))

        (make-empty-file "/tmp/test")
        (unwind-protect
            (progn (should t))
          (when (get-buffer "test")
            (kill-buffer "test")
            (push "test" bydi--temp-files))
          (delete-file "/tmp/test"))))))

(ert-deftest bydi--report ()
  (bydi (message)
    (let ((bydi--temp-files nil))
      (bydi--report)
      (bydi-was-not-called message)

      (setq bydi--temp-files '("/tmp/test"))
      (bydi--report)
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

(ert-deftest bydi-path-setup ()
  (let ((load-path nil)
        (default-directory "/tmp"))

    (bydi ((:mock getenv :with ignore))

      (bydi-path-setup (list "test" "mock"))

      (should (equal load-path '("/tmp/mock" "/tmp/test" "/tmp"))))))

(ert-deftest bydi-ert-runner-setup ()
  (bydi (add-hook)

    (bydi-ert-runner-setup 'always)

    (bydi-was-called-nth-with add-hook '(ert-runner-reporter-run-ended-functions bydi--report) 0)
    (bydi-was-called-nth-with add-hook '(ert-runner-reporter-run-ended-functions always) 1)))

(defvar coverage-file (ert-resource-file "coverage.txt"))

(ert-deftest bydi--matches-in-string ()
  (let ((str "This 1 string has 3 matches, or is it 2?")
        (pattern "\\(?1:[[:digit:]]\\)"))

    (should (equal '("2" "3" "1") (bydi--matches-in-string pattern str)))))

(ert-deftest bydi-calculate-coverage ()
  (let ((bydi-report--text-file coverage-file))

    (should (string-equal "Combined coverage: 37.33%" (bydi-calculate-coverage)))))

(ert-deftest bydi-calculate-coverage--errors-on-missing-file ()
  (let ((bydi-report--text-file "/tmp/non-existence.txt"))

    (should-error (bydi-calculate-coverage) :type 'user-error)))

;;; bydi-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
