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

Each item in TO-MOCK can either be a function symbol returning
nil, a cons cell of shape (FUN . REPLACE) returning the result of
calling REPLACE, a plist of shape (:mock FUN :with REPLACE)
returning the result of calling REPLACE or a plist of
shape (:mock FUN :return VAL) returning VAL."
  (declare (indent defun))

  `(cl-letf* ((bydi-mock-history (make-hash-table :test 'equal))
              ,@(mapcar (lambda (it)
                          (cond
                           ((bydi-with-mock--valid-plistp it)
                            (cond
                             ((plist-get it :return)
                              (bydi-with-mock--bind (plist-get it :mock)
                                                    (plist-get it :return)))
                             ((plist-get it :with)
                              (bydi-with-mock--bind (plist-get it :mock)
                                                    `(apply #',(plist-get it :with) r)))))
                           ((consp it)
                            (bydi-with-mock--bind (car it) `(apply ,(cdr it) r)))
                           (t
                            (bydi-with-mock--bind it))))
                        (if (listp to-mock) to-mock (list to-mock))))
     ,@body))

(defun bydi-with-mock--valid-plistp (plist)
  "Check if PLIST list a valid one."
  (and (plistp plist)
       (memq :mock plist)
       (or (memq :return plist)
           (memq :with plist))))

(defun bydi-with-mock--remember (fun args)
  "Remember function FUN and return ARGS."
  (let* ((prev (gethash fun bydi-mock-history))
         (val (if prev (push args prev) (list args))))

    (puthash fun val bydi-mock-history)
    args))

(defun bydi-with-mock--bind (fun &optional return)
  "Return template to override FUN.

Optionally, return RETURN."
  `((symbol-function ',fun)
    (lambda (&rest r)
      (interactive)
      (apply 'bydi-with-mock--remember (list ',fun r))
      ,return)))

(defalias 'bydi 'bydi-with-mock)

(defun bydi-clear-mocks ()
  "Clear mock history."
  (setq bydi-mock-history (make-hash-table :test 'equal)))

(defmacro bydi-was-called (fun)
  "Check if mocked FUN was called."
  `(let ((actual (gethash ',fun bydi-mock-history 'not-called)))
     (should-not (equal 'not-called actual))))

(defmacro bydi-was-called-with (fun expected)
  "Check if FUN was called with EXPECTED."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (car (gethash ',fun bydi-mock-history))))))

(defmacro bydi-was-called-nth-with (fun expected index)
  "Check if FUN was called with EXPECTED on the INDEXth call."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (nth ,index (reverse (gethash ',fun bydi-mock-history)))))))

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

(defvar bydi-report--text-file "./coverage/results.txt"
  "The file used to store text coverage.")

(defvar bydi-report--json-file "./coverage/.resultset.json"
  "The file used to store the JSON coverage.")

(defun bydi--matches-in-string (regexp str)
  "Return all matches of REGEXP in STR."
  (let ((matches nil))

    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string 1) matches)))
    matches))

(defun bydi-coverage--add (buf type)
  "Add all numbers of TYPE in buffer BUF."
  (let* ((regex (concat type ": \\(?1:[[:digit:]]+\\)"))
         (content (with-current-buffer buf (buffer-string)))
         (numbers (bydi--matches-in-string regex content)))

    (apply '+ (mapcar #'string-to-number numbers))))

(defun bydi-coverage--average ()
  "Calculate the average."
  (with-temp-buffer
    (insert-file-contents bydi-report--text-file)

    (when-let* ((relevant (bydi-coverage--add (current-buffer) "Relevant"))
                (covered (bydi-coverage--add (current-buffer) "Covered")))

      (string-to-number (format "%.2f%%" (* 100 (/ (float covered) relevant)))))))

;; Integration

(defvar bydi-env--coverage-with-json "COVERAGE_WITH_JSON"
  "If set, SimpleCov (JSON) format is used.")

(defvar bydi-env--ci "CI"
  "Set if in a CI environment.")

(defvar bydi-env--github-workspace "GITHUB_WORKSPACE"
  "Location of the project in GitHub action.")

(defvar undercover-force-coverage)
(defvar undercover--merge-report)
(declare-function undercover--setup "ext:undercover.el")

(defun bydi-undercover-setup (patterns)
  "Set up `undercover' for PATTERNS."
  (when (require 'undercover nil t)
    (message "Setting up `undercover' with %s" patterns)

    (let ((report-format 'text)
          (report-file bydi-report--text-file))

      (setq undercover-force-coverage t)

      (cond
       ((getenv bydi-env--ci)
        (setq report-format 'lcov
              report-file nil))

       ((getenv bydi-env--coverage-with-json)
        (setq undercover--merge-report nil
              report-format 'simplecov
              report-file bydi-report--json-file)))

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
  (let* ((source-dir (expand-file-name (or (getenv bydi-env--github-workspace)
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

  (when reporter
    (add-hook
     'ert-runner-reporter-run-ended-functions
     reporter)))

;;;###autoload
(defun bydi-calculate-coverage ()
  "Calculate the coverage using the results file."
  (interactive)

  (if (file-exists-p bydi-report--text-file)
      (let ((average (bydi-coverage--average)))

        (message "Combined coverage: %s%%" average))
    (user-error "Text report %s doesn't exist" bydi-report--text-file)))

(provide 'bydi)

;;; bydi.el ends here
