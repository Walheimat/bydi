;;; bydi.el --- Mocking macros -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/bydi
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: extensions

;;; Commentary:
;;
;; `bydi' allows you to mock (or spy on) functions and verify their
;; call history in various ways. You can provide your own mock
;; implementation, define their return value or use a short-hand.

;;; Code:

(require 'ert-x)
(require 'cl-lib)
(require 'compat nil t)

(defmacro bydi-with-mock (to-mock &rest body)
  "Evaluate BODY mocking list of function(s) TO-MOCK.

TO-MOCK maybe be a single item or a list of items.

The arguments passed to the mocked functions will be recorded in
a hash table. Repeated calls will append results.

Each item in TO-MOCK can either be a function symbol returning
the result of `bydi-mock--remember', a plist of shape (:mock FUN
:with REPLACE) returning the result of calling REPLACE, a plist
of shape (:mock FUN :return VAL) returning VAL, a plist of
shape (:ignore FUN) that will replace FUN with `ignore', a plist
of shape (:always FUN) that will replace FUN with `always', a
plist of shape (:sometimes FUN) that will return the value of
`bydi-mock-sometimes', a plist of shape (:spy FUN) that will
advise FUN so that its invocations are recorded, or a cons cell
of shape (FUN . REPLACE) returning the result of calling
REPLACE."
  (declare (indent defun))

  (let ((instructions (if (listp to-mock) to-mock (list to-mock))))

    `(cl-letf* ((bydi-mock-history (make-hash-table :test 'equal))
                (bydi-mock-sometimes t)
                (bydi-spies ',(cl-loop for i in instructions
                                       when (and (bydi-mock--valid-plistp i)
                                                 (plist-member i :spy))
                                       collect (plist-get i :spy)))
                ,@(delq nil
                        (mapcar (lambda (it)
                                  (cl-destructuring-bind (bind to) (bydi-mock--binding it)
                                    (when bind
                                      (unless (bydi--good-mock-p bind)
                                        (bydi-warn--bad-mock bind))
                                      (bydi-mock--bind bind to))))
                                instructions)))
       (bydi-spy--create)
       ,@body
       (bydi-spy--clear))))

;; Call verification

(defmacro bydi-was-called (fun)
  "Check if mocked FUN was called."
  `(let ((actual (gethash ',fun bydi-mock-history 'not-called)))
     (should (bydi--was-called ',fun nil actual))))

(defmacro bydi-was-not-called (fun)
  "Check if mocked FUN was not called."
  `(let ((actual (gethash ',fun bydi-mock-history 'not-called)))
     (should (bydi--was-not-called ',fun nil actual))))

(defmacro bydi-was-called-with (fun expected)
  "Check if FUN was called with EXPECTED."
  (let ((safe-exp (bydi--safe-exp expected)))

    `(let ((actual (gethash ',fun bydi-mock-history)))
       (should (bydi--was-called-with ',fun ,safe-exp (car actual))))))

(defmacro bydi-was-called-nth-with (fun expected index)
  "Check if FUN was called with EXPECTED on the INDEXth call."
  (let ((safe-exp (bydi--safe-exp expected)))

    `(let ((actual (nth ,index (reverse (gethash ',fun bydi-mock-history)))))
       (should (bydi--was-called-with ',fun ,safe-exp actual)))))

(defmacro bydi-was-called-n-times (fun expected)
  "Check if mocked FUN was called EXPECTED times."
  `(let ((actual (length (gethash ',fun bydi-mock-history))))
     (should (bydi--was-called-n-times ',fun ,expected actual))))


(defun bydi--was-called (_fun _expected actual)
  "Verify that ACTUAL represents a function call."
  (not (equal 'not-called actual)))

(defun bydi--was-not-called (_fun _expected actual)
  "Verify that ACTUAL represents missing function call."
  (equal 'not-called actual))

(defun bydi--was-called-with (_fun expected actual)
  "Verify that EXPECTED represents ACTUAL arguments."
  (equal expected actual))

(defun bydi--was-called-n-times (_fun expected actual)
  "Verify that EXPECTED number matches ACTUAL."
  (eq expected actual))

;; Matching

(defmacro bydi-match-expansion (form &rest value)
  "Match expansion of FORM against VALUE."
  `(should (bydi-match-expansion--matches ',form ,@value)))

(defun bydi-match-expansion--matches (form value)
  "Make sure FORM matches VALUE."
  (eval
   `(pcase (macroexpand-1 ',form)
      (',value t))))

;; History:

(defvar bydi-mock-history nil)

(defun bydi-mock--remember (fun args)
  "Remember function FUN and return ARGS."
  (let* ((prev (gethash fun bydi-mock-history))
         (val (if prev (push args prev) (list args))))

    (puthash fun val bydi-mock-history)
    args))

;; Binding

(defun bydi-mock--binding (mock)
  "Get function and binding for MOCK."
  (cond
   ((bydi-mock--valid-plistp mock)
    (cond
     ((plist-member mock :return)
      `(,(plist-get mock :mock) ,(plist-get mock :return)))
     ((plist-member mock :with)
      `(,(plist-get mock :mock) (apply #',(plist-get mock :with) r)))
     ((plist-member mock :spy)
      '(nil nil))

     ;; Short-hands.
     ((plist-member mock :ignore)
      `(,(plist-get mock :ignore) (apply #'ignore r)))
     ((plist-member mock :always)
      `(,(plist-get mock :always) (apply #'always r)))
     ((plist-member mock :sometimes)
      `(,(plist-get mock :sometimes) (funcall #'bydi-mock--sometimes)))))
   ((consp mock)
    `(,(car mock) (apply ,(cdr mock) r)))
   (t `(,mock nil))))

(defun bydi-mock--bind (fun &optional return)
  "Return template to override FUN.

Optionally, return RETURN."
  (if return
      `((symbol-function ',fun)
        (lambda (&rest r)
          (interactive)
          (apply 'bydi-mock--remember (list ',fun r))
          ,return))
    `((symbol-function ',fun)
      (lambda (&rest r)
        (interactive)
        (apply 'bydi-mock--remember (list ',fun r))))))

(defun bydi-mock--valid-plistp (plist)
  "Check if PLIST list a valid one."
  (and (plistp plist)
       (or (and (memq :mock plist)
                (or (memq :return plist)
                    (memq :with plist)))
           (memq :spy plist)
           (memq :always plist)
           (memq :ignore plist)
           (memq :sometimes plist))))

(defvar bydi--never-mock '(fboundp advice-add advice-remove file-exists-p)
  "Functions that, when mocked, do or may prevent test execution.")

(defun bydi--good-mock-p (to-mock)
  "Check if TO-MOCK is mockable."
  (not (memq to-mock bydi--never-mock)))

(defun bydi-warn--bad-mock (to-mock)
  "Warn about using TO-MOCK in a mock."
  (display-warning
   'bydi
   (format "Mocking %s may lead to issues" to-mock)
   :warning))

;; Spying

(defvar bydi-spies nil)

(defvar bydi-spy--advice-name 'bydi-spi)

(defun bydi-spy--create ()
  "Record invocations of FUN in history."
  (mapc (lambda (it)
          (advice-add
           it :after
           (lambda (&rest args)
             (apply 'bydi-mock--remember (list it args)))
           (list (cons 'name bydi-spy--advice-name))))
        bydi-spies))

(defun bydi-spy--clear ()
  "Clear all spies."
  (mapc (lambda (it) (advice-remove it bydi-spy--advice-name)) bydi-spies))

;; Toggling

(defvar bydi-mock-sometimes nil)

(defun bydi-mock--sometimes ()
  "Return value of `bydi-mock-sometimes'."
  bydi-mock-sometimes)

;; Explaining

(defun bydi--readable (data)
  "Make sure DATA is readable."
  (cond
   ((null data)
    'null)
   (t data)))

(defun bydi-explain--explain-actual (fun expected actual)
  "Explain that FUN was called with ACTUAL not EXPECTED."
  (if (equal actual 'not-called)
      `(no-call ',fun)
    `(call ',fun
           :expected ,(bydi--readable expected)
           :actual ,(bydi--readable actual))))

(put 'bydi--was-called 'ert-explainer 'bydi-explain--explain-actual)
(put 'bydi--was-not-called 'ert-explainer 'bydi-explain--explain-actual)
(put 'bydi--was-called-with 'ert-explainer 'bydi-explain--explain-actual)
(put 'bydi--was-called-n-times 'ert-explainer 'bydi-explain--explain-actual)

(defun bydi-explain--explain-mismatch (a b)
  "Explain that A didn't match B."
  `(no-match :wanted ,(macroexpand-1 a) :got ,b))

(put 'bydi-match-expansion--matches 'ert-explainer 'bydi-explain--explain-mismatch)

;; Other macros

(defun bydi-rf (a &rest _r)
  "Return first argument passed A."
  a)

(defun bydi-ra (&rest r)
  "Return all arguments R."
  r)

(defun bydi-rt (&rest _r)
  "Return symbol `testing'."
  'testing)

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

;; Reporting

(defun bydi-coverage--report (&rest _)
  "Print created temp files."
  (when bydi--temp-files
    (message
     "\nCreated the following temp files:\n%s"
     bydi--temp-files)))

(defvar bydi-coverage--text-file "./coverage/results.txt"
  "The file used to store text coverage.")

(defvar bydi-coverage--json-file "./coverage/.resultset.json"
  "The file used to store the JSON coverage.")

(defun bydi-coverage--add (buf type)
  "Add all numbers of TYPE in buffer BUF."
  (let* ((regex (concat type ": \\(?1:[[:digit:]]+\\)"))
         (content (with-current-buffer buf (buffer-string)))
         (numbers (bydi--matches-in-string regex content)))

    (apply '+ (mapcar #'string-to-number numbers))))

(defun bydi-coverage--average ()
  "Calculate the average."
  (with-temp-buffer
    (insert-file-contents bydi-coverage--text-file)

    (when-let* ((relevant (bydi-coverage--add (current-buffer) "Relevant"))
                (covered (bydi-coverage--add (current-buffer) "Covered")))

      (string-to-number (format "%.2f%%" (* 100 (/ (float covered) relevant)))))))

;; Utility

(defun bydi--safe-exp (sexp)
  "Get SEXP as a quoted list."
  (if (listp sexp) sexp `(list ,sexp)))

(defun bydi--matches-in-string (regexp str)
  "Return all matches of REGEXP in STR."
  (let ((matches nil))

    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string 1) matches)))
    matches))

;; Setup helpers

(defvar bydi-setup--env-coverage-with-json "COVERAGE_WITH_JSON"
  "If set, SimpleCov (JSON) format is used.")

(defvar bydi-setup--env-ci "CI"
  "Set if in a CI environment.")

(defvar bydi-setup--env-github-workspace "GITHUB_WORKSPACE"
  "Location of the project in GitHub action.")

(defun bydi-setup--paths (paths)
  "Set up `load-path'.

Optionally, set up additional relative PATHS.

This function returns a list of the directories added to the
`load-path'."
  (let* ((source-dir (expand-file-name (or (getenv bydi-setup--env-github-workspace)
                                           default-directory)))
         (paths (append (list source-dir) (mapcar (lambda (it) (expand-file-name it source-dir)) paths))))

    (message "Adding %s to `load-path'" paths)

    (dolist (it paths)
      (add-to-list 'load-path it))

    paths))

(defun bydi-setup--ert-runner (reporter)
  "Set up `ert-runner'.

An optional REPORTER function can be passed."
  (add-hook
   'ert-runner-reporter-run-ended-functions
   #'bydi-coverage--report)

  (when reporter
    (add-hook
     'ert-runner-reporter-run-ended-functions
     reporter)))

(defvar undercover-force-coverage)
(defvar undercover--merge-report)
(declare-function undercover--setup "ext:undercover.el")

(defun bydi-setup--undercover (patterns)
  "Set up `undercover' for PATTERNS."
  (when (require 'undercover nil t)
    (message "Setting up `undercover' with %s" patterns)

    (let ((report-format 'text)
          (report-file bydi-coverage--text-file))

      (setq undercover-force-coverage t)

      (cond
       ((getenv bydi-setup--env-ci)
        (setq report-format 'lcov
              report-file nil))

       ((getenv bydi-setup--env-coverage-with-json)
        (setq undercover--merge-report nil
              report-format 'simplecov
              report-file bydi-coverage--json-file)))

      (undercover--setup
       (append patterns
               (list
                (list :report-format report-format)
                (list :report-file report-file)
                (list :send-report nil)))))))

;; API

(defun bydi-clear-mocks ()
  "Clear mock history."
  (setq bydi-mock-history (make-hash-table :test 'equal)))

(defun bydi-toggle-sometimes (&optional no-clear)
  "Toggle `bydi-mock-sometimes'.

Unless NO-CLEAR is t, this also calls `bydi-clea-mocks'."
  (setq bydi-mock-sometimes (not bydi-mock-sometimes))

  (unless no-clear
    (bydi-clear-mocks)))

;;;###autoload
(defalias 'bydi 'bydi-with-mock)

;;;###autoload
(defun bydi-path-setup (&optional paths)
  "Set up `load-path'.

Optionally, set up additional relative PATHS.

This function returns a list of the directories added to the
`load-path'."
  (bydi-setup--paths paths))

;;;###autoload
(defun bydi-ert-runner-setup (&optional reporter)
  "Set up `ert-runner'.

An optional REPORTER function can be passed."
  (bydi-setup--ert-runner reporter))

;;;###autoload
(defun bydi-undercover-setup (patterns)
  "Set up `undercover' for PATTERNS."
  (bydi-setup--undercover patterns))

;;;###autoload
(defun bydi-calculate-coverage ()
  "Calculate the coverage using the results file."
  (interactive)

  (if (file-exists-p bydi-coverage--text-file)
      (let ((average (bydi-coverage--average)))

        (message "Combined coverage: %s%%" average))
    (user-error "Text report %s doesn't exist" bydi-coverage--text-file)))

(provide 'bydi)

;;; bydi.el ends here
