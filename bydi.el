;;; bydi.el --- Mocking macros -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/bydi
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: extensions

;;; Commentary:
;;
;; `bydi' is a library of mocking macros used in `ert' tests.
;;
;; With it you can mock (almost) any function and command, allowing
;; you to provide your own mock implementation or define a return
;; value. Some common mock implementations, using `ignore' and
;; `always' for conditionals, have shorthands.
;;
;; You can also spy on functions to just record invocations without
;; providing a return value or implementation.
;;
;; You can also watch variables.
;;
;; Finally, you can verify invocations and assignments in various
;; ways using macros similar to `should'.

;;; Code:

(require 'ert-x)
(require 'cl-lib)
(require 'compat nil t)

;;; -- Variables

(defvar bydi--history nil
  "An alist of the form (SYMBOL . (ARGS-1 ARGS-2 ...)).

For functions, each ARGS is a list of the arguments it was called
with. For variables, it's the value it was set to.

The verification functions use this list to inspect invocation
and assignment results.")

(defvar bydi-mock--risky '(fboundp advice-add advice-remove file-exists-p)
  "List of risky functions.

These are functions that, when mocked, do or may prevent test
execution.")

(defvar bydi-mock--sometimes nil
  "Value for mocks using `:sometimes' shorthand.

All such functions will return this value. It will be set to t at
the beginning and can be toggled using `bydi-toggle-sometimes'.")

(defvar bydi-expect--elision '\...
  "Symbol indicating an elision during argument verification.

Allows only verifying some of the arguments passed to a mocked
function.")

(defvar bydi-spy--spies nil
  "List of functions spied upon during `bydi-with-mock'.

Each function will be advised to record the arguments it was
called with.")

(defvar bydi-spy--advice-name 'bydi-spi
  "Name used for the advising of spied upon functions.

Allows removing anonymous advice.")

(defvar bydi-watch--watchers nil
  "List of variables watched during `bydi-with-mock'.

Each variable will be watched to record the values assigned to
it.")

;;; -- Macros

(defmacro bydi-with-mock (to-mock &rest body)
  "Evaluate a form with mocks.

TO-MOCK is a list of symbols to mock. These can be functions or
variables. It maybe be a single item or a list of items.

The arguments passed to the mocked functions will be recorded in
a hash table. Repeated calls will append results.

Each item in TO-MOCK can either be a function symbol returning
the result of `bydi--record', a plist of shape (:mock FUN :with
REPLACE) returning the result of calling REPLACE, a plist of
shape (:mock FUN :return VAL) returning VAL, a plist of
shape (:ignore FUN) that will replace FUN with `ignore', a plist
of shape (:always FUN) that will replace FUN with `always', a
plist of shape (:sometimes FUN) that will return the value of
`bydi-mock--sometimes', a plist of shape (:spy FUN) that will
advise FUN so that its invocations are recorded, a plist of
shape (:watch VAR) that will watch VAR so assignments are
recorded, or a cons cell of shape (FUN . REPLACE) returning the
result of calling REPLACE.

BODY is the form evaluated while the mocking is in place. Any
verfication macro `bydi-was-*' needs to be part of this form."
  (declare (indent defun))

  (let ((instructions (if (listp to-mock) to-mock (list to-mock))))

    `(cl-letf* ((bydi--history (make-hash-table :test 'equal))

                (bydi-spy--spies ',(bydi-mock--collect instructions :spy))
                (bydi-watch--watchers ',(bydi-mock--collect instructions :watch))

                (bydi-mock--sometimes t)

                ,@(delq nil
                        (mapcar (lambda (it)
                                  (cl-destructuring-bind (bind to) (bydi-mock--binding it)
                                    (when bind
                                      (bydi-mock--check bind it)
                                      (bydi-mock--bind bind to))))
                                instructions)))

       (bydi--setup)
       ,@body
       (bydi--teardown))))

(defmacro bydi-was-called (fun)
  "Check if mocked FUN was called."
  `(let ((actual (gethash ',fun bydi--history 'not-called)))
     (should (bydi-verify--was-called ',fun nil actual))))

(defmacro bydi-was-not-called (fun)
  "Check if mocked FUN was not called."
  `(let ((actual (gethash ',fun bydi--history 'not-called)))
     (should (bydi-verify--was-not-called ',fun nil actual))))

(defmacro bydi-was-called-with (fun expected)
  "Check if FUN was called with EXPECTED."
  (declare (indent defun))

  `(let ((actual (gethash ',fun bydi--history)))
     (should (bydi-verify--was-called-with ',fun ,expected (car actual)))))

(defmacro bydi-was-called-nth-with (fun expected index)
  "Check if FUN was called with EXPECTED on the INDEXth call."
  `(let ((actual (nth ,index (reverse (gethash ',fun bydi--history)))))
     (should (bydi-verify--was-called-with ',fun ,expected actual))))

(defmacro bydi-was-called-last-with (fun expected)
  "Check if FUN was called with EXPECTED on the last call."
  `(let ((actual (car-safe (last (reverse (gethash ',fun bydi--history))))))
     (should (bydi-verify--was-called-with ',fun ,expected actual))))

(defmacro bydi-was-called-n-times (fun expected)
  "Check if mocked FUN was called EXPECTED times."
  `(let ((actual (length (gethash ',fun bydi--history))))
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

;;; -- Handlers

(defun bydi--record (sym args)
  "Record SYM and return ARGS."
  (let* ((prev (gethash sym bydi--history))
         (val (if prev (push args prev) (list args))))

    (puthash sym val bydi--history)
    args))

(defun bydi--setup ()
  "Set up spies and watchers."
  (bydi-spy--create)
  (bydi-watch--create))

(defun bydi--teardown ()
  "Tear down spies and watchers."
  (bydi-spy--clear)
  (bydi-watch--clear))

(defun bydi--warn (message &rest args)
  "Emit a warning.

The MESSAGE will be formatted with ARGS."
  (display-warning
   'bydi
   (apply #'format message args)
   :warning))

;;; -- Verification

(defun bydi-verify--was-called (_fun _expected actual)
  "Verify that ACTUAL represents a function call."
  (not (equal 'not-called actual)))

(defun bydi-verify--was-not-called (_fun _expected actual)
  "Verify that ACTUAL represents missing function call."
  (equal 'not-called actual))

(defun bydi-verify--was-called-with (_fun expected actual)
  "Verify that EXPECTED represents ACTUAL arguments.

If the EXPECTED value start with `bydi-expect--elision', the check only
extends to verifying that expected argument is in expected
arguments in the order given."
  (let ((safe-exp (bydi-verify--safe-exp expected)))

    (cond
     ((memq bydi-expect--elision safe-exp)
      (let ((args safe-exp)
            (matches t)
            (last-match -1))

        (while (and matches args)
          (let* ((it (car args))
                 (this-match (seq-position actual it)))

            (unless (eq it bydi-expect--elision)
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

(defalias 'bydi-was-set 'bydi-was-called)
(defalias 'bydi-was-not-set 'bydi-was-not-called)
(defalias 'bydi-was-set-to 'bydi-was-called-with)
(defalias 'bydi-was-not-set-to 'bydi-was-not-called-with)
(defalias 'bydi-was-set-nth-to 'bydi-was-called-nth-with)
(defalias 'bydi-was-set-last-to 'bydi-was-called-last-with)
(defalias 'bydi-was-set-n-times 'bydi-was-called-n-times)

;;; -- Mocking

(defun bydi-mock--binding (mock)
  "Get function and binding for MOCK."
  (cond
   ((bydi-mock--valid-plistp mock)
    (cond
     ((plist-member mock :return)
      (unless (plist-get mock :return)
        (bydi--warn "Returning 'nil' may lead to unexpected results"))
      `(,(or (plist-get mock :mock) (plist-get mock :risky-mock)) ,(or (plist-get mock :return))))
     ((plist-member mock :with)
      `(,(or (plist-get mock :mock) (plist-get mock :risky-mock)) (apply #',(plist-get mock :with) r)))
     ((or (plist-member mock :spy) (plist-member mock :watch))
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
          (apply 'bydi--record (list ',fun r))
          ,return))
    `((symbol-function ',fun)
      (lambda (&rest r)
        (interactive)
        (apply 'bydi--record (list ',fun r))))))

(defun bydi-mock--collect (instructions prop)
  "Collect PROP entries from INSTRUCTIONS."
  (cl-loop for i in instructions
           when (and (bydi-mock--valid-plistp i)
                     (plist-member i prop))
           collect (plist-get i prop)))

(defun bydi-mock--valid-plistp (plist)
  "Check if PLIST list a valid one."
  (and (plistp plist)
       (or (and (or (memq :mock plist) (memq :risky-mock plist))
                (or (memq :return plist)
                    (memq :with plist)))
           (memq :spy plist)
           (memq :watch plist)
           (memq :always plist)
           (memq :ignore plist)
           (memq :sometimes plist))))

(defun bydi-mock--sometimes ()
  "Return value of `bydi-mock--sometimes'."
  bydi-mock--sometimes)

(defun bydi-mock--check (fun instruction)
  "Verify binding FUN using INSTRUCTION."
  (when (and (memq fun bydi-mock--risky)
             (not (memq :risky-mock instruction)))

    (bydi--warn "Mocking '%s' may lead to issues" fun)))

;;; -- Spying

(defun bydi-spy--create ()
  "Record invocations of FUN in history."
  (mapc (lambda (it)
          (advice-add
           it :after
           (lambda (&rest args)
             (apply 'bydi--record (list it args)))
           (list (cons 'name bydi-spy--advice-name))))
        bydi-spy--spies))

(defun bydi-spy--clear ()
  "Clear all spies."
  (mapc (lambda (it) (advice-remove it bydi-spy--advice-name)) bydi-spy--spies))

;;; -- Watching

(defun bydi-watch--watcher (symbol newval _operation _where)
  "Record that SYMBOL was updated with NEWVAL."
  (bydi--record symbol (list newval)))

(defun bydi-watch--create ()
  "Record settings of symbols."
  (mapc (lambda (it)
          (add-variable-watcher it #'bydi-watch--watcher))
        bydi-watch--watchers))

(defun bydi-watch--clear ()
  "Clear watchers."
  (mapc
   (lambda (it) (remove-variable-watcher it #'bydi-watch--watcher))
   bydi-watch--watchers))

;;; -- Explaining

(defun bydi-explain--explain-actual (fun expected actual)
  "Explain that FUN was called with ACTUAL not EXPECTED."
  (if (equal actual 'not-called)
      `(no-call ',fun)
    `(call ',fun
           :reason ,(ert--explain-equal-rec expected actual)
           :expected ,(bydi-explain--make-readable expected)
           :actual ,(bydi-explain--make-readable actual))))

(put 'bydi-verify--was-called 'ert-explainer 'bydi-explain--explain-actual)
(put 'bydi-verify--was-not-called 'ert-explainer 'bydi-explain--explain-actual)
(put 'bydi-verify--was-called-with 'ert-explainer 'bydi-explain--explain-actual)
(put 'bydi-verify--was-called-n-times 'ert-explainer 'bydi-explain--explain-actual)

(defun bydi-explain--explain-mismatch (a b)
  "Explain that A didn't match B."
  (let ((expanded (macroexpand-1 a)))

    `(no-match
      :reason ,(ert--explain-equal-rec expanded b)
      :wanted expanded
      :got ,b)))

(put 'bydi-verify--matches 'ert-explainer 'bydi-explain--explain-mismatch)

(defun bydi-explain--make-readable (data)
  "Make sure DATA is readable."
  (cond
   ((null data)
    'null)
   (t data)))

;;; -- API

(defun bydi-clear-mocks ()
  "Clear mock history."
  (clrhash bydi--history))

(defun bydi-toggle-sometimes (&optional no-clear)
  "Toggle `bydi-mock--sometimes'.

Unless NO-CLEAR is t, this also calls `bydi-clear-mocks'."
  (setq bydi-mock--sometimes (not bydi-mock--sometimes))

  (unless no-clear
    (bydi-clear-mocks)))

;;;###autoload
(defalias 'bydi 'bydi-with-mock)

(provide 'bydi)

;;; bydi.el ends here
