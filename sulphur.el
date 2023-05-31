;;; sulphur.el --- Test macros and setups -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/sulphur
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: extensions

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'ert-x)
(require 'compat nil t)

;; Macros

(defun sulphur-rf (a &rest _r)
  "Return first argument passed A."
  a)

(defun sulphur-ra (&rest r)
  "Return all arguments R."
  r)

(defun sulphur-rt (&rest _r)
  "Return symbol `testing'."
  'testing)

(defvar sulphur-mock-history nil)

(defmacro sulphur-with-mock (to-mock &rest body)
  "Evaluate BODY mocking list of function(s) TO-MOCK.

TO-MOCK maybe be a single item or a list of items.

The arguments passed to the mocked functions will be recorded in
a hash table. Repeated calls will append results.

Each item in TO-MOCK can either be a function symbol or a cons
cell of shape (FUNCTION . MOCK-IMPLEMENTATION). The return value
is either the argument list or the result of the mock
implementation."
  (declare (indent defun))

  `(cl-letf* ((sulphur-mock-history (make-hash-table :test 'equal))
              (remember (lambda (fun args)
                          (let* ((prev (gethash fun sulphur-mock-history))
                                 (val (if prev (push args prev) (list args))))
                            (puthash fun val sulphur-mock-history)
                            args)))
              ,@(mapcar (lambda (it)
                          (cond
                           ((consp it)
                            `((symbol-function ',(car it))
                              (lambda (&rest r)
                                (interactive)
                                (apply remember (list ',(car it) r))
                                (apply ,(cdr it) r))))
                           (t
                            `((symbol-function ',it)
                              (lambda (&rest r)
                                (interactive)
                                (apply remember (list ',it r)))))))
                        (if (listp to-mock) to-mock (list to-mock))))
     ,@body))

(defun sulphur-clear-mocks ()
  "Clear mock history."
  (setq sulphur-mock-history (make-hash-table :test 'equal)))

(defmacro sulphur-was-called-with (fun expected)
  "Check if FUN was called with EXPECTED."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (car (gethash ',fun sulphur-mock-history))))))

(defmacro sulphur-was-called-nth-with (fun expected index)
  "Check if FUN was called with EXPECTED on the INDEXth call."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (nth ,index (reverse (gethash ',fun sulphur-mock-history)))))))

(defmacro sulphur-was-called (fun)
  "Check if mocked FUN was called."
  `(let ((actual (gethash ',fun sulphur-mock-history 'not-called)))
     (should-not (equal 'not-called actual))))

(defmacro sulphur-was-not-called (fun)
  "Check if mocked FUN was not called."
  `(let ((actual (gethash ',fun sulphur-mock-history 'not-called)))
     (should (equal 'not-called actual))))

(defmacro sulphur-was-called-n-times (fun expected)
  "Check if mocked FUN was called EXPECTED times."
  `(should (equal ,expected (length (gethash ',fun sulphur-mock-history)))))

(defmacro sulphur-match-expansion (form &rest value)
  "Match expansion of FORM against VALUE."
  `(should (pcase (macroexpand-1 ',form)
             ,@(mapcar #'(lambda (x) (list x t)) value))))

(cl-defmacro sulphur-should-every (forms &key check expected)
  "CHECK if all FORMS have EXPECTED value using CHECK."
  (declare (indent defun))
  (let ((check (or check 'eq)))

    `(progn ,@(mapcar (lambda (it) `(should (,check ,it ,expected))) forms))))

(defvar sulphur-test-helper--temp-files nil)

(defmacro sulphur-with-temp-file (filename &rest body)
  "Create and discard a file.

FILENAME is the name of the file, BODY the form to execute while
the file is alive.

The associated file buffer is also killed."
  (declare (indent defun))

  (let ((tmp-file (expand-file-name filename "/tmp")))

    `(progn
       (let ((sulphur-tmp-file ,tmp-file))

         (make-empty-file ,tmp-file)

         (unwind-protect
             (progn ,@body)
           (when (get-buffer ,filename)
             (kill-buffer ,filename)
             (push ,filename sulphur-test-helper--temp-files))
           (delete-file ,tmp-file))))))

;; Integration

(defvar undercover-force-coverage)
(defvar undercover--merge-report)
(declare-function undercover--setup "ext:undercover.el")

(defun sulphur-undercover-setup (patterns)
  "Set up `undercover' for PATTERNS."
  (when (require 'undercover nil t)
    (message "Setting up `undercover'")

    (let ((report-format 'text)
          (report-file "./coverage/results.txt"))

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
       (append patterns
               (list
                (list :report-format report-format)
                (list :report-file report-file)
                (list :send-report nil)))))))

(defun sulphur-path-setup ()
  "Set up paths."
  (let* ((source-dir (expand-file-name (or (getenv "GITHUB_WORKSPACE")
                                           default-directory))))

    (message "Setting source path to %s" source-dir)

    (add-to-list 'load-path source-dir)))

(defun sulphur-test-helper--report (&rest _)
  "Print created temp files."
  (when sulphur-test-helper--temp-files
    (message
     "\nCreated the following temp files:\n%s"
     sulphur-test-helper--temp-files)))

(defun sulphur-ert-runner-setup ()
  "Set up `ert-runner'."
  (when (require 'ert-runner nil t)
    (add-hook
     'ert-runner-reporter-run-ended-functions
     #'sulphur-test-helper--report)))

(provide 'sulphur)

;;; sulphur.el ends here
