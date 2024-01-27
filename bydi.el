;;; bydi.el --- Mocking macros -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/bydi
;; Version: 0.6.5
;; Package-Requires: ((emacs "29.1"))
;; Keywords: extensions

;;; Commentary:
;;
;; `bydi' is a framework to stump, mock and spy on functions during
;; the execution of `ert' tests.
;;
;; You can selectively mock (almost) any function and command by
;; providing your own mock implementation or simply defining what
;; should be returned.
;;
;; You can also spy on functions to just record invocations without
;; providing a return value or replacement implementation.
;;
;; You can also watch variables.
;;
;; Finally, you can verify invocations and assignments in various
;; ways using macros similar to `should'.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'compat nil t)

;;;; Variables

(defvar bydi--history nil
  "An alist of the form (SYMBOL . (ARGS-1 ARGS-2 ...)).

For functions, each ARGS is a list of the arguments it was called
with. For variables, it's the values it was set to.

The verification functions use this list to inspect invocation
and assignment results.")

(defvar bydi-mock--risky '(fboundp advice-add advice-remove file-exists-p)
  "List of risky functions.

These are functions that, when mocked, do or may prevent test
execution.")

(defvar bydi-mock--always nil
  "List of functions that will return t when called.

Can be toggled using `bydi-toggle-volatile' or
`bydi-toggle-sometimes'.")

(defvar bydi-mock--ignore nil
  "List of functions that will return nil when called.

Can be toggled using `bydi-toggle-volatile' or
`bydi-toggle-sometimes'.")

(defvar bydi-expect--elision '\...
  "Symbol indicating an elision during argument verification.

Allows verifying only those arguments passed to a mocked function
that are of interest.")

(defvar bydi-spy--spies nil
  "List of functions spied upon during `bydi--mock'.

Each function will be advised to record the arguments it was
called with.")

(defvar bydi-spy--advice-name 'bydi-spi
  "Name used for the advising of spied upon functions.

Allows removing anonymous advice.")

(defvar bydi-watch--watchers nil
  "List of variables watched during `bydi--mock'.

Each variable will be watched to record the values assigned to
it.")

(defvar bydi--when nil
  "Hash table of selective mocking of spies.")

;;;; Macros

(defmacro bydi--mock (to-mock &rest body)
  "Evaluate a form with mocks.

TO-MOCK is a list of symbols to mock. These can be functions or
variables. It maybe be a single item or a list of items.

The arguments passed to the mocked functions or assigned to the
watched variables are recorded in a hash table. Repeated calls or
assignments will append results.

Each item in TO-MOCK can either be a function symbol returning
the result of `bydi--record', a plist of shape (:mock FUN :with
REPLACE) returning the result of calling REPLACE, a plist of
shape (:mock FUN :return VAL) returning VAL, a plist of
shape (:ignore FUN) that will replace FUN with `ignore', a plist
of shape (:always FUN) that will replace FUN with `always', a
plist of shape (:sometimes FUN) that will return t unless
`bydi-mock-toggle-volatile' is called , a plist of
shape (:othertimes FUN) that will do the inverse, a plist of
shape (:spy FUN) that will advise FUN so that its invocations are
recorded with its routine untouched, a plist of shape (:watch
VAR) that will watch VAR so assignments are recorded, or a cons
cell of shape (FUN . REPLACE) returning the result of calling
REPLACE.

BODY is the form evaluated while the mocking, spying and watching
is in place. Any `bydi-was-*' verification macro needs to be part
of this form."
  (declare (indent defun))

  (let ((instructions (if (listp to-mock) to-mock (list to-mock))))

    `(cl-letf* ((bydi--history (make-hash-table :test 'equal))
                (bydi--when (make-hash-table :test 'equal))

                (bydi-spy--spies ',(bydi-mock--collect instructions :spy))
                (bydi-watch--watchers ',(bydi-mock--collect instructions :watch))

                (bydi-mock--always ',(bydi-mock--collect instructions :sometimes))
                (bydi-mock--ignore ',(bydi-mock--collect instructions :othertimes))

                ,@(bydi-mock--mocks instructions))

       (unwind-protect

           (progn
             (bydi--setup)
             ,@body)

         (bydi--teardown)))))

;;;; Calling macros

(defmacro bydi-was-called (fun &optional clear)
  "Check if mocked FUN was called.

If CLEAR is t, clear the history of calls of that function."
  `(let ((actual (gethash ',fun bydi--history 'not-called)))
     ,@(delq
        nil
        `((should (bydi-verify--was-called ',fun 'called actual))
          ,(when clear `(bydi-clear-mocks-for ',fun))))))

(defmacro bydi-was-not-called (fun)
  "Check if mocked FUN was not called."
  `(let ((actual (gethash ',fun bydi--history 'not-called)))
     (should (bydi-verify--was-not-called ',fun 'not-called actual))))

(defmacro bydi-was-called-with (fun expected &optional clear)
  "Check if FUN was called with EXPECTED.

If CLEAR is t, clear the history of calls of that function."
  (declare (indent defun))

  `(let ((expected (bydi-verify--safe-exp ,expected))
         (actual (car-safe (gethash ',fun bydi--history))))
     ,@(delq
        nil
        `((should (bydi-verify--was-called-with ',fun expected actual))
          ,(when clear `(bydi-clear-mocks-for ',fun))))))

(defmacro bydi-was-called-nth-with (fun expected index)
  "Check if FUN was called with EXPECTED on the INDEXth call."
  `(let ((expected (bydi-verify--safe-exp ,expected))
         (actual (nth ,index (reverse (gethash ',fun bydi--history)))))
     (should (bydi-verify--was-called-with ',fun expected actual))))

(defmacro bydi-was-called-last-with (fun expected)
  "Check if FUN was called with EXPECTED on the last call."
  `(let ((expected (bydi-verify--safe-exp ,expected))
         (actual (car-safe (last (reverse (gethash ',fun bydi--history))))))
     (should (bydi-verify--was-called-with ',fun expected actual))))

(defmacro bydi-was-called-n-times (fun expected)
  "Check if mocked FUN was called EXPECTED times."
  `(let ((actual (length (gethash ',fun bydi--history))))
     (should (bydi-verify--was-called-n-times ',fun ,expected actual))))

;;;; Setting macros

(defmacro bydi-was-set-to (var to &optional clear)
  "Check that VAR was set to TO.

If CLEAR is t, clear the history of assignments to that variable."
  `(let ((actual (car-safe (gethash ',var bydi--history))))
     ,@(delq
        nil
        `((should (bydi-verify--was-set-to ',var ,to actual))
          ,(when clear `(bydi-clear-mocks-for ',var))))))

(defmacro bydi-was-set-to-nth (var to index)
  "Check that VAR was set to TO during INDEXth setting."
  `(let ((actual (nth ,index (reverse (gethash ',var bydi--history)))))
     (should (bydi-verify--was-set-to ',var ,to actual))))

(defmacro bydi-was-set-to-last (var to)
  "Check that VAR was set to TO during last setting."
  `(let ((actual (car-safe (last (reverse (gethash ',var bydi--history))))))
     (should (bydi-verify--was-set-to ',var ,to actual))))

(defmacro bydi-was-set-n-times (var expected)
  "Verify that VAR was set EXPECTED times."
  `(let ((actual (length (gethash ',var bydi--history))))

     (should (bydi-verify--was-set-n-times ',var ,expected actual))))

(defmacro bydi-was-set (var &optional clear)
  "Check if VAR was set.

If CLEAR is t, clear the history of assignments to that variable."
  `(let ((actual (gethash ',var bydi--history 'not-set)))
     ,@(delq
        nil
        `((should (bydi-verify--was-set ',var 'set actual))
          ,(when clear `(bydi-clear-mocks-for ',var))))))

(defmacro bydi-was-not-set (var)
  "Check that VAR was not set."
  `(let ((actual (gethash ',var bydi--history 'not-set)))

     (should-not (bydi-verify--was-set ',var 'not-set actual))))

;;;; Other macros

(defmacro bydi-match-expansion (form &rest value)
  "Match expansion of FORM against VALUE."
  `(should (bydi-verify--matches ',form ,@value)))

(cl-defmacro bydi-should-every (forms &key check expected)
  "CHECK if all FORMS have EXPECTED value using CHECK."
  (declare (indent defun))
  (let ((check (or check 'eq)))

    `(progn ,@(mapcar (lambda (it) `(should (,check ,it ,expected))) forms))))

;;;; Helpers

(defun bydi-return-first (a &rest _r)
  "Return first argument passed A."
  a)
(defalias 'bydi-rf 'bydi-return-first)

(defun bydi-return-all (&rest r)
  "Return all arguments R."
  r)
(defalias 'bydi-ra 'bydi-return-all)

(defun bydi-return-testing (&rest _r)
  "Return symbol `testing'."
  'testing)
(defalias 'bydi-rt 'bydi-return-testing)

;;;; Handlers

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

;;;; Verification

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

(defun bydi-verify--was-set-to (_var exp-to to)
  "Verify that expected and actual settings match.

Matches EXP-FROM against FROM and EXP-TO against TO."
  (equal exp-to to))

(defun bydi-verify--was-set (_var _expected actual)
  "Verify that variable was set.

This is done by checking that ACTUAL is not the symbol `not-set'."
  (not (equal actual 'not-set)))

(defun bydi-verify--was-set-n-times (_var expected actual)
  "Verify that EXPECTED matches ACTUAL settings."
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

(defun bydi-verify--unwrap-single-item (a b)
  "If A and B are both single-item lists, unwrap them."
  (or (and (listp a)
           (listp b)
           (= 1 (length a) (length b))
           (list (nth 0 a) (nth 0 b)))
      (list a b)))

;;;; Mocking

(defun bydi-mock--mocks (instructions)
  "Get mocks for INSTRUCTIONS."
  (delq nil (mapcar (lambda (it)
                      (cl-destructuring-bind (bind to) (bydi-mock--binding it)
                        (when bind
                          (bydi-mock--check bind it)
                          (bydi-mock--bind bind to))))
                    instructions)))

(defun bydi-mock--binding (mock)
  "Get function and binding for MOCK."
  (cond
   ((bydi-mock--valid-plistp mock)
    (cond
     ;; Returning constant value or variable.
     ((plist-member mock :return)
      (unless (plist-get mock :return)
        (bydi--warn "Returning `nil' may lead to unexpected results"))
      `(,(or (plist-get mock :mock) (plist-get mock :risky-mock)) ,(plist-get mock :return)))

     ;; Signaling.
     ((plist-member mock :fail)
      `(,(plist-get mock :fail)
        ,(let ((type (or (plist-get mock :with) 'signal)))

           `(apply #',type
                   ,(or (plist-get mock :args)
                        (pcase type
                          ('user-error
                           ''("User error"))
                          ('signal
                           ''(error "Lisp error"))))))))

     ;; Replacing implementation.
     ((plist-member mock :with)
      `(,(or (plist-get mock :mock) (plist-get mock :risky-mock)) (apply #',(plist-get mock :with) r)))

     ;; Ignore spying and watching.
     ((or (plist-member mock :spy) (plist-member mock :watch))
      '(nil nil))

     ;; Short-hands.
     ((plist-member mock :ignore)
      `(,(plist-get mock :ignore) (apply #'ignore r)))
     ((plist-member mock :always)
      `(,(plist-get mock :always) (apply #'always r)))

     ;; Volatile.
     ((or (plist-member mock :sometimes) (plist-member mock :othertimes))
      (let ((fun (or (plist-get mock :sometimes) (plist-get mock :othertimes))))
        `(,fun (funcall #'bydi-mock--volatile ',fun))))))

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
           (memq :fail plist)
           (memq :spy plist)
           (memq :watch plist)
           (memq :always plist)
           (memq :ignore plist)
           (memq :sometimes plist)
           (memq :othertimes plist))))

(defun bydi-mock--volatile (fun)
  "Check if FUN should return t."
  (and (memq fun bydi-mock--always) t))

(defun bydi-mock--check (fun instruction)
  "Verify binding FUN using INSTRUCTION."
  (when (and (memq fun bydi-mock--risky)
             (not (memq :risky-mock instruction)))

    (bydi--warn "Mocking `%s' may lead to issues" fun)))

;;;; Spying

(defun bydi-spy--create ()
  "Record invocations of FUN in history."
  (mapc (lambda (it)
          (advice-add
           it :around
           (lambda (fun &rest args)

             (apply 'bydi--record (list it args))

             (or (apply 'bydi-spy--when (append (list it) args))
                 (apply fun args)))
           (list (cons 'name bydi-spy--advice-name))))
        bydi-spy--spies))

(defun bydi-spy--clear ()
  "Clear all spies."
  (mapc (lambda (it) (advice-remove it bydi-spy--advice-name)) bydi-spy--spies))

(defun bydi-spy--when (fun &rest args)
  "Maybe return recorded value for FUN.

If ARGS match the the IN field of the recorded value, the value
of OUT will be returned. If it was recorded with ONCE being t,
the recording is removed before returning the OUT value."
  (and-let* ((condition (gethash fun bydi--when))
             ((equal args (plist-get condition :called-with))))

    (when-let (rem (plist-get condition :once))
      (remhash fun bydi--when))

    (plist-get condition :then-return)))

(cl-defmacro bydi-when--answer (fun &key called-with then-return once)
  "Return THEN-RETURN when FUN is called with CALLED-WITH.

If ONCE is to, only do this once."
  `(progn
     (unless (memq ',fun bydi-spy--spies)
       (bydi--warn "No spy for `%s' was recorded" ',fun))
     (puthash
      ',fun
      (list :called-with ,called-with :then-return ,then-return :once ,once)
      bydi--when)))

;;;; Watching

(defun bydi-watch--watcher (symbol newval operation _where)
  "Record that SYMBOL was updated with NEWVAL.

Only records when OPERATION is a let or set binding."
  (when (memq operation '(let set))
    (bydi--record symbol newval)))

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

;;;; Explaining

(defun bydi-explain--wrong-call (fun expected actual)
  "Explain that FUN was called with ACTUAL not EXPECTED."
  (cond
   ((equal expected 'not-called)
    `(was-called ',fun :args ,actual))

   ((eq expected 'called)
    `(never-called ',fun))

   (t
    `(wrong-arguments
      ',fun
      :reason ,(cl-destructuring-bind (a b)
                   (bydi-verify--unwrap-single-item expected actual)
                 (ert--explain-equal-rec a b))))))

(defun bydi-explain--wrong-setting (var expected actual)
  "Explain that VAR was set to ACTUAL, not EXPECTED."
  (cond
   ((eq expected 'not-set)
    `(was-set ',var :to ,actual))

   ((eq expected 'set)
    `(never-set ',var))

   (t
    `(wrong-setting
      ',var
      :reason ,(ert--explain-equal-rec expected actual)))))

(defun bydi-explain--explain-mismatch (actual expected)
  "Explain that ACTUAL didn't match EXPECTED."
  (let ((actual (macroexpand-1 actual)))

    `(no-match
      :reason ,(ert--explain-equal-rec expected actual)
      :wanted ,expected
      :got ,actual)))

(put 'bydi-verify--was-called 'ert-explainer 'bydi-explain--wrong-call)
(put 'bydi-verify--was-not-called 'ert-explainer 'bydi-explain--wrong-call)
(put 'bydi-verify--was-called-with 'ert-explainer 'bydi-explain--wrong-call)
(put 'bydi-verify--was-called-n-times 'ert-explainer 'bydi-explain--wrong-call)

(put 'bydi-verify--was-set 'ert-explainer 'bydi-explain--wrong-setting)
(put 'bydi-verify--was-not-set 'ert-explainer 'bydi-explain--wrong-setting)
(put 'bydi-verify--was-set-to 'ert-explainer 'bydi-explain--wrong-setting)
(put 'bydi-verify--was-set-n-times 'ert-explainer 'bydi-explain--wrong-setting)

(put 'bydi-verify--matches 'ert-explainer 'bydi-explain--explain-mismatch)

;;;; API

(defun bydi-clear-mocks ()
  "Clear all mocks.

This will clear the entire history (which is shared by functions
and variables)."
  (clrhash bydi--history))

(defun bydi-clear-mocks-for (symbol)
  "Clear mocks for SYMBOL.

SYMBOL can be the name of a function or a variable."
  (remhash symbol bydi--history))

(defun bydi-toggle-sometimes (&optional no-clear)
  "Toggle all volatile functions.

Unless NO-CLEAR is t, this also calls `bydi-clear-mocks-for' for
all functions."
  (dolist (it (append bydi-mock--always bydi-mock--ignore))
    (bydi-toggle-volatile it no-clear)))

(defun bydi-toggle-volatile (fun &optional no-clear)
  "Toggle volatile FUN.

If this function previously returned t, it will now return nil
and vice versa.

Unless NO-CLEAR is t, this also calls `bydi-clear-mocks' for this
function."
  (cond
   ((memq fun bydi-mock--always)

    (setq bydi-mock--always (delete fun bydi-mock--always))
    (push fun bydi-mock--ignore))
   ((memq fun bydi-mock--ignore)

    (setq bydi-mock--ignore (delete fun bydi-mock--ignore))
    (push fun bydi-mock--always)))

  (unless no-clear
    (bydi-clear-mocks-for fun)))

;;;###autoload
(defalias 'bydi 'bydi--mock)

;;;###autoload
(defalias 'bydi-with-mock 'bydi--mock)

(defalias 'bydi-when 'bydi-when--answer)

(provide 'bydi)

;;; bydi.el ends here
