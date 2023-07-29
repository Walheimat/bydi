;;; bydi.el --- Mocking macros -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/bydi
;; Version: 0.4.0
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

(require 'bydi-ci nil t)

;;; -- Variables

(defvar bydi--temp-files nil)
(defvar bydi-mock-history nil)
(defvar bydi--never-mock '(fboundp advice-add advice-remove file-exists-p)
  "Functions that, when mocked, do or may prevent test execution.")
(defvar bydi-spies nil)
(defvar bydi--elision '\...)

(defvar bydi-spy--advice-name 'bydi-spi)
(defvar bydi-mock-sometimes nil)

;;; -- Macros

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
                                      (bydi-mock--check bind)
                                      (bydi-mock--bind bind to))))
                                instructions)))
       (bydi-spy--create)
       ,@body
       (bydi-spy--clear))))

(defmacro bydi-was-called (fun)
  "Check if mocked FUN was called."
  `(let ((actual (gethash ',fun bydi-mock-history 'not-called)))
     (should (bydi-verify--was-called ',fun nil actual))))

(defmacro bydi-was-not-called (fun)
  "Check if mocked FUN was not called."
  `(let ((actual (gethash ',fun bydi-mock-history 'not-called)))
     (should (bydi-verify--was-not-called ',fun nil actual))))

(defmacro bydi-was-called-with (fun expected)
  "Check if FUN was called with EXPECTED."
  (declare (indent defun))

  `(let ((actual (gethash ',fun bydi-mock-history)))
     (should (bydi-verify--was-called-with ',fun ,expected (car actual)))))

(defmacro bydi-was-called-nth-with (fun expected index)
  "Check if FUN was called with EXPECTED on the INDEXth call."
  `(let ((actual (nth ,index (reverse (gethash ',fun bydi-mock-history)))))
     (should (bydi-verify--was-called-with ',fun ,expected actual))))

(defmacro bydi-was-called-last-with (fun expected)
  "Check if FUN was called with EXPECTED on the last call."
  `(let ((actual (car-safe (last (reverse (gethash ',fun bydi-mock-history))))))
     (should (bydi-verify--was-called-with ',fun ,expected actual))))

(defmacro bydi-was-called-n-times (fun expected)
  "Check if mocked FUN was called EXPECTED times."
  `(let ((actual (length (gethash ',fun bydi-mock-history))))
     (should (bydi-verify--was-called-n-times ',fun ,expected actual))))

(defmacro bydi-match-expansion (form &rest value)
  "Match expansion of FORM against VALUE."
  `(should (bydi-verify--matches ',form ,@value)))

(cl-defmacro bydi-should-every (forms &key check expected)
  "CHECK if all FORMS have EXPECTED value using CHECK."
  (declare (indent defun))
  (let ((check (or check 'eq)))

    `(progn ,@(mapcar (lambda (it) `(should (,check ,it ,expected))) forms))))

;;; -- Helpers

(defun bydi-rf (a &rest _r)
  "Return first argument passed A."
  a)

(defun bydi-ra (&rest r)
  "Return all arguments R."
  r)

(defun bydi-rt (&rest _r)
  "Return symbol `testing'."
  'testing)

;;; -- Verification

(defun bydi-verify--was-called (_fun _expected actual)
  "Verify that ACTUAL represents a function call."
  (not (equal 'not-called actual)))

(defun bydi-verify--was-not-called (_fun _expected actual)
  "Verify that ACTUAL represents missing function call."
  (equal 'not-called actual))

(defun bydi-verify--was-called-with (_fun expected actual)
  "Verify that EXPECTED represents ACTUAL arguments.

If the EXPECTED value start with `bydi--elision', the check only
extends to verifying that expected argument is in expected
arguments in the order given."
  (let ((safe-exp (bydi-verify--safe-exp expected)))

    (cond
     ((memq bydi--elision safe-exp)
      (let ((args safe-exp)
            (matches t)
            (last-match -1))

        (while (and matches args)
          (let* ((it (car args))
                 (this-match (seq-position actual it)))

            (unless (eq it bydi--elision)
              (if (and this-match
                       (> this-match last-match))
                  (setq last-match this-match)
                (setq matches nil)))
            (setq args (cdr args))))
        matches))
     ((eq (length safe-exp) (length actual))
      (equal safe-exp actual))
     ((null expected)
      (null actual))
     (t nil))))

(defun bydi-verify--was-called-n-times (_fun expected actual)
  "Verify that EXPECTED number matches ACTUAL."
  (eq expected actual))

(defun bydi-verify--matches (form value)
  "Make sure FORM matches VALUE."
  (eval
   `(pcase (macroexpand-1 ',form)
      (',value t))))


(defun bydi-verify--safe-exp (sexp)
  "Get SEXP as a quoted list."
  (cond
   ((null sexp)
    (list nil))
   ((listp sexp)
    sexp)
   (t (list sexp))))

;;; -- Mocking

(defun bydi-mock--remember (fun args)
  "Remember function FUN and return ARGS."
  (let* ((prev (gethash fun bydi-mock-history))
         (val (if prev (push args prev) (list args))))

    (puthash fun val bydi-mock-history)
    args))

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

(defun bydi-mock--sometimes ()
  "Return value of `bydi-mock-sometimes'."
  bydi-mock-sometimes)

(defun bydi-mock--check (fun)
  "Verify binding FUN."
  (unless (not (memq fun bydi--never-mock))
    (display-warning
     'bydi
     (format "Mocking %s may lead to issues" fun)
     :warning)))

;;; -- Spying

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

;;; -- Explaining

(defun bydi-explain--explain-actual (fun expected actual)
  "Explain that FUN was called with ACTUAL not EXPECTED."
  (if (equal actual 'not-called)
      `(no-call ',fun)
    `(call ',fun
           :expected ,(bydi-explain--make-readable expected)
           :actual ,(bydi-explain--make-readable actual))))

(put 'bydi-verify--was-called 'ert-explainer 'bydi-explain--explain-actual)
(put 'bydi-verify--was-not-called 'ert-explainer 'bydi-explain--explain-actual)
(put 'bydi-verify--was-called-with 'ert-explainer 'bydi-explain--explain-actual)
(put 'bydi-verify--was-called-n-times 'ert-explainer 'bydi-explain--explain-actual)

(defun bydi-explain--explain-mismatch (a b)
  "Explain that A didn't match B."
  `(no-match :wanted ,(macroexpand-1 a) :got ,b))

(put 'bydi-verify--matches 'ert-explainer 'bydi-explain--explain-mismatch)

(defun bydi-explain--make-readable (data)
  "Make sure DATA is readable."
  (cond
   ((null data)
    'null)
   (t data)))

;;; -- API

;;;###autoload
(defun bydi-clear-mocks ()
  "Clear mock history."
  (setq bydi-mock-history (make-hash-table :test 'equal)))

;;;###autoload
(defun bydi-toggle-sometimes (&optional no-clear)
  "Toggle `bydi-mock-sometimes'.

Unless NO-CLEAR is t, this also calls `bydi-clea-mocks'."
  (setq bydi-mock-sometimes (not bydi-mock-sometimes))

  (unless no-clear
    (bydi-clear-mocks)))

;;;###autoload
(defalias 'bydi 'bydi-with-mock)

(provide 'bydi)

;;; bydi.el ends here
