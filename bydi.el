;;; bydi.el --- Test macros and setups -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/bydi
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

(defun bydi-rf (a &rest _r)
  "Return first argument passed A."
  a)

(defun bydi-ra (&rest r)
  "Return all arguments R."
  r)

(defun bydi-rt (&rest _r)
  "Return symbol `testing'."
  'testing)

(defvar bydi-mock-history nil)

(defmacro bydi-with-mock (to-mock &rest body)
  "Evaluate BODY mocking list of function(s) TO-MOCK.

TO-MOCK maybe be a single item or a list of items.

The arguments passed to the mocked functions will be recorded in
a hash table. Repeated calls will append results.

Each item in TO-MOCK can either be a function symbol or a cons
cell of shape (FUNCTION . MOCK-IMPLEMENTATION). The return value
is either the argument list or the result of the mock
implementation."
  (declare (indent defun))

  `(cl-letf* ((bydi-mock-history (make-hash-table :test 'equal))
              (remember (lambda (fun args)
                          (let* ((prev (gethash fun bydi-mock-history))
                                 (val (if prev (push args prev) (list args))))
                            (puthash fun val bydi-mock-history)
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

(defun bydi-clear-mocks ()
  "Clear mock history."
  (setq bydi-mock-history (make-hash-table :test 'equal)))

(defmacro bydi-was-called-with (fun expected)
  "Check if FUN was called with EXPECTED."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (car (gethash ',fun bydi-mock-history))))))

(defmacro bydi-was-called-nth-with (fun expected index)
  "Check if FUN was called with EXPECTED on the INDEXth call."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (nth ,index (reverse (gethash ',fun bydi-mock-history)))))))

(defmacro bydi-was-called (fun)
  "Check if mocked FUN was called."
  `(let ((actual (gethash ',fun bydi-mock-history 'not-called)))
     (should-not (equal 'not-called actual))))

(defmacro bydi-was-not-called (fun)
  "Check if mocked FUN was not called."
  `(let ((actual (gethash ',fun bydi-mock-history 'not-called)))
     (should (equal 'not-called actual))))

(defmacro bydi-was-called-n-times (fun expected)
  "Check if mocked FUN was called EXPECTED times."
  `(should (equal ,expected (length (gethash ',fun bydi-mock-history)))))

(defmacro bydi-match-expansion (form &rest value)
  "Match expansion of FORM against VALUE."
  `(should (pcase (macroexpand-1 ',form)
             ,@(mapcar #'(lambda (x) (list x t)) value))))

(cl-defmacro bydi-should-every (forms &key check expected)
  "CHECK if all FORMS have EXPECTED value using CHECK."
  (declare (indent defun))
  (let ((check (or check 'eq)))

    `(progn ,@(mapcar (lambda (it) `(should (,check ,it ,expected))) forms))))

(defvar bydi--temp-files nil)

(defmacro bydi-with-temp-file (filename &rest body)
  "Create and discard a file.

FILENAME is the name of the file, BODY the form to execute while
the file is alive.

The associated file buffer is also killed."
  (declare (indent defun))

  (let ((tmp-file (expand-file-name filename "/tmp")))

    `(progn
       (let ((bydi-tmp-file ,tmp-file))

         (make-empty-file ,tmp-file)

         (unwind-protect
             (progn ,@body)
           (when (get-buffer ,filename)
             (kill-buffer ,filename)
             (push ,filename bydi--temp-files))
           (delete-file ,tmp-file))))))

(defun bydi--report (&rest _)
  "Print created temp files."
  (when bydi--temp-files
    (message
     "\nCreated the following temp files:\n%s"
     bydi--temp-files)))

;; Integration

(defvar undercover-force-coverage)
(defvar undercover--merge-report)
(declare-function undercover--setup "ext:undercover.el")

(defun bydi-undercover-setup (patterns)
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

(defun bydi-path-setup (&optional paths)
  "Set up `load-path'.

Optionally, set up additional relative PATHS.

This function returns a list of the directories added to the
`load-path'."
  (let* ((source-dir (expand-file-name (or (getenv "GITHUB_WORKSPACE")
                                           default-directory)))
         (paths (append (list source-dir) (mapcar (lambda (it) (expand-file-name it source-dir)) paths))))

    (message "Adding %s to `load-path'" paths)

    (dolist (it paths)
      (add-to-list 'load-path it))

    paths))

(defun bydi-ert-runner-setup (&optional reporter)
  "Set up `ert-runner'.

An optional REPORTER function can be passed."
  (add-hook
   'ert-runner-reporter-run-ended-functions
   #'bydi--report)

  (add-hook
   'ert-runner-reporter-run-ended-functions
   reporter))

(provide 'bydi)

;;; bydi.el ends here
