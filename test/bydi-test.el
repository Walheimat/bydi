;;; bydi-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'bydi)

(ert-deftest bydi-rf ()
  (should (equal (bydi-rf 'test 'this 'now) 'test)))

(ert-deftest bydi-ra ()
  (should (equal (bydi-ra 'test 'this 'now) '(test this now))))

(ert-deftest bydi-rt ()
  (should (equal (bydi-rt 'test 'this 'now) 'testing)))

(ert-deftest bydi--mock--simple ()
  (bydi-match-expansion
   (bydi--mock (bydi-rf bydi-ra)
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi--when (make-hash-table :test 'equal))
         (bydi-spy--spies 'nil)
         (bydi-watch--watchers 'nil)
         (bydi-mock--always 'nil)
         (bydi-mock--ignore 'nil)
         (bydi-mock--vars 'nil)
         ((symbol-function 'bydi-rf)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'bydi-rf r))))
         ((symbol-function 'bydi-ra)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'bydi-ra r)))))
      (unwind-protect
          (progn
            (bydi--setup)
            (should (always)))
        (bydi--teardown)))))

(ert-deftest bydi--mock--old-usage ()
  (bydi-match-expansion
   (bydi--mock ((bydi-rt . #'ignore)
                    (format . (lambda (a &rest _args) a)))
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi--when (make-hash-table :test 'equal))
         (bydi-spy--spies 'nil)
         (bydi-watch--watchers 'nil)
         (bydi-mock--always 'nil)
         (bydi-mock--ignore 'nil)
         (bydi-mock--vars 'nil)
         ((symbol-function 'bydi-rt)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'bydi-rt r))
            (apply #'ignore r)))
         ((symbol-function 'format)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'format r))
            (apply (lambda (a &rest _args) a) r))))
      (unwind-protect
          (progn
            (bydi--setup)
            (should (always)))
        (bydi--teardown)))))

(ert-deftest bydi--mock--explicit ()
  (bydi-match-expansion
   (bydi--mock ((:return "hello" :mock substring)
                (:risky-mock buffer-file-name :return "/tmp/test.el")
                (:risky-mock bydi-ra :with ignore)
                (:with always :mock buffer-live-p)
                (:fail forward-line :with user-error))
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi--when (make-hash-table :test 'equal))
         (bydi-spy--spies 'nil)
         (bydi-watch--watchers 'nil)
         (bydi-mock--always 'nil)
         (bydi-mock--ignore 'nil)
         (bydi-mock--vars 'nil)
         ((symbol-function 'substring)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'substring r))
            "hello"))
         ((symbol-function 'buffer-file-name)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'buffer-file-name r))
            "/tmp/test.el"))
         ((symbol-function 'bydi-ra)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'bydi-ra r))
            (apply #'ignore r)))
         ((symbol-function 'buffer-live-p)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'buffer-live-p r))
            (apply #'always r)))
         ((symbol-function 'forward-line)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'forward-line r))
            (apply #'user-error '("User error")))))
      (unwind-protect
          (progn
            (bydi--setup)
            (should (always)))
        (bydi--teardown)))))

(ert-deftest bydi--mock--spies-and-watchers ()
  (bydi-match-expansion
   (bydi--mock ((:spy buffer-live-p)
                (:mock abbrev-table-p :with bydi-rt)
                (:spy derived-mode-p)
                (:watch major-mode))
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi--when (make-hash-table :test 'equal))
         (bydi-spy--spies '(buffer-live-p derived-mode-p))
         (bydi-watch--watchers '(major-mode))
         (bydi-mock--always 'nil)
         (bydi-mock--ignore 'nil)
         (bydi-mock--vars 'nil)
         ((symbol-function 'abbrev-table-p)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'abbrev-table-p r))
            (apply #'bydi-rt r))))

      (unwind-protect
          (progn
            (bydi--setup)
            (should (always)))
        (bydi--teardown)))))

(ert-deftest bydi--mock--shorthands ()
  (bydi-match-expansion
   (bydi--mock ((:ignore buffer-live-p)
                (:always abbrev-table-p)
                (:sometimes derived-mode-p))
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi--when (make-hash-table :test 'equal))
         (bydi-spy--spies 'nil)
         (bydi-watch--watchers 'nil)
         (bydi-mock--always '(derived-mode-p))
         (bydi-mock--ignore 'nil)
         (bydi-mock--vars 'nil)
         ((symbol-function 'buffer-live-p)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'buffer-live-p r))
            (apply #'ignore r)))
         ((symbol-function 'abbrev-table-p)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'abbrev-table-p r))
            (apply #'always r)))
         ((symbol-function 'derived-mode-p)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'derived-mode-p r))
            (funcall #'bydi-mock--volatile 'derived-mode-p))))
      (unwind-protect
          (progn
            (bydi--setup)
            (should (always)))
        (bydi--teardown)))))

(ert-deftest bydi--mock--single-function ()
  (bydi-match-expansion
   (bydi--mock bydi-rf
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi--when (make-hash-table :test 'equal))
         (bydi-spy--spies 'nil)
         (bydi-watch--watchers 'nil)
         (bydi-mock--always 'nil)
         (bydi-mock--ignore 'nil)
         (bydi-mock--vars 'nil)
         ((symbol-function 'bydi-rf)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'bydi-rf r)))))
      (unwind-protect
          (progn
            (bydi--setup)
            (should (always)))
        (bydi--teardown)))))

(ert-deftest bydi--mock--unmockable ()
  (bydi--mock (bydi-mock--check)
    (let ((bydi-mock--risky '(new-line)))
      (bydi-match-expansion
       (bydi--mock ((:ignore new-line))
         nil)
       '(cl-letf*
            ((bydi--history
              (make-hash-table :test 'equal))
             (bydi--when (make-hash-table :test 'equal))
             (bydi-spy--spies 'nil)
             (bydi-watch--watchers 'nil)
             (bydi-mock--always 'nil)
             (bydi-mock--ignore 'nil)
             (bydi-mock--vars 'nil)
             ((symbol-function 'new-line)
              (lambda
                (&rest r)
                (interactive)
                (apply 'bydi--record
                       (list 'new-line r))
                (apply #'ignore r))))
          (unwind-protect
              (progn
                (bydi--setup)
                nil)
            (bydi--teardown)))))
    (bydi-was-called bydi-mock--check)))

(ert-deftest bydi--mock--returning-nil ()
  (bydi--mock (bydi--warn)
    (bydi-match-expansion
     (bydi--mock ((:mock ignore :return nil))
       nil)
     '(cl-letf*
         ((bydi--history
           (make-hash-table :test 'equal))
          (bydi--when (make-hash-table :test 'equal))
          (bydi-spy--spies 'nil)
          (bydi-watch--watchers 'nil)
          (bydi-mock--always 'nil)
          (bydi-mock--ignore 'nil)
          (bydi-mock--vars 'nil)
          ((symbol-function 'ignore)
           (lambda
             (&rest r)
             (interactive)
             (apply 'bydi--record
                    (list 'ignore r))
             nil)))
        (unwind-protect
            (progn
              (bydi--setup)
              nil)
          (bydi--teardown))))

    (bydi-was-called-with bydi--warn "Returning `nil' may lead to unexpected results")))

(ert-deftest bydi-clear-mocks ()
  (let ((bydi--history (make-hash-table :test 'equal)))

    (bydi--record 'test 'testing)

    (should (eq 1 (length (hash-table-keys bydi--history))))

    (bydi-clear-mocks)

    (should (eq 0 (length (hash-table-keys bydi--history))))))

(ert-deftest bydi-clear-mocks-for ()
  (let ((bydi--history (make-hash-table :test 'equal)))

    (bydi--record 'test 'testing)
    (bydi--record 'check 'checking)

    (bydi-clear-mocks-for 'test)

    (should (eq 1 (length (hash-table-keys bydi--history))))))

(ert-deftest bydi-mock--check ()
  (bydi display-warning
    (let ((bydi-mock--risky '(ignore)))

      (bydi-mock--check 'ignore '(:mock ignore :return nil))
      (bydi-was-called-with display-warning (list 'bydi "Mocking `ignore' may lead to issues" :warning))

      (bydi-clear-mocks)
      (bydi-mock--check 'ignore '(:risky-mock ignore :return nil))
      (bydi-was-not-called display-warning))))

(ert-deftest bydi-toggle-sometimes ()
  (bydi ((:sometimes buffer-live-p)
         (:othertimes hash-table-p)
         (:always y-or-n-p)
         (:spy bydi-clear-mocks-for))

    (should (buffer-live-p))
    (should-not (hash-table-p))
    (should (y-or-n-p))

    (bydi-toggle-sometimes)

    (should-not (buffer-live-p))
    (should (hash-table-p))

    (should (gethash 'y-or-n-p bydi--history))

    (bydi-was-called bydi-clear-mocks-for)

    ;; Need to clear manually here.
    (setq bydi--history (make-hash-table :test 'equal))
    (bydi-toggle-sometimes t)

    (should (buffer-live-p))
    (bydi-was-not-called bydi-clear-mocks-for)))

(ert-deftest bydi-toggle-volatile ()
  (bydi ((:sometimes buffer-live-p)
         (:sometimes hash-table-p)
         (:othertimes y-or-n-p))

    (should (buffer-live-p))
    (should (hash-table-p))
    (should-not (y-or-n-p))

    (bydi-toggle-volatile 'hash-table-p)

    (should (buffer-live-p))
    (should-not (hash-table-p))
    (should-not (y-or-n-p))

    (bydi-toggle-volatile 'y-or-n-p)

    (should (buffer-live-p))
    (should-not (hash-table-p))
    (should (y-or-n-p))))

(ert-deftest bydi-was-called ()
  (bydi-match-expansion
   (bydi-was-called apply)
   '(let ((actual (gethash 'apply bydi--history 'not-called)))
      (should (bydi-verify--was-called 'apply 'called actual))))

  (bydi-match-expansion
   (bydi-was-called apply t)
   '(let ((actual (gethash 'apply bydi--history 'not-called)))
      (should (bydi-verify--was-called 'apply 'called actual))
      (bydi-clear-mocks-for 'apply))))

(ert-deftest bydi-was-called-with ()
  (bydi-match-expansion
   (bydi-was-called-with apply '(a b c))
   '(let ((expected (bydi-verify--safe-exp '(a b c)))
          (actual (car-safe (gethash 'apply bydi--history))))
     (should (bydi-verify--was-called-with 'apply expected actual))))

  (bydi-match-expansion
   (bydi-was-called-with apply '(a b c) t)
   '(let ((expected (bydi-verify--safe-exp '(a b c)))
          (actual (car-safe (gethash 'apply bydi--history))))
      (should (bydi-verify--was-called-with 'apply expected actual))
      (bydi-clear-mocks-for 'apply))))

(ert-deftest bydi-was-called-with--single-item ()
  (bydi-match-expansion
   (bydi-was-called-with apply "test")
   '(let ((expected (bydi-verify--safe-exp "test"))
          (actual (car-safe (gethash 'apply bydi--history))))
      (should (bydi-verify--was-called-with 'apply expected actual)))))

(ert-deftest bydi-was-called-with--partial-matching ()
  (let ((actual '(a b c d))
        (bydi-expect--elision '\...))

    (should (bydi-verify--was-called-with nil '(a b c d) actual))
    (should (bydi-verify--was-called-with nil '(... b c d) actual))
    (should (bydi-verify--was-called-with nil '(... b d) actual))
    (should (bydi-verify--was-called-with nil '(a ... d) actual))
    (should (bydi-verify--was-called-with nil '(... c) actual))
    (should-not (bydi-verify--was-called-with nil '(... b a) actual))))

(ert-deftest bydi-was--clearing-history ()
  (bydi (forward-line)
    (forward-line)
    (bydi-was-called forward-line t)
    (bydi-was-not-called forward-line)))

(ert-deftest bydi-verify--was-called-with--nil-matching ()
  (should (bydi-verify--was-called-with nil nil nil)))

(ert-deftest bydi-was-called-nth-with ()
  (bydi-match-expansion
   (bydi-was-called-nth-with apply 'test 1)
   '(let ((expected (bydi-verify--safe-exp 'test))
          (actual (nth 1 (reverse (gethash 'apply bydi--history)))))
      (should (bydi-verify--was-called-with 'apply expected actual)))))

(ert-deftest bydi-was-called-nth-with--single-item ()
  (bydi-match-expansion
   (bydi-was-called-nth-with apply "test" 1)
   '(let ((expected (bydi-verify--safe-exp "test"))
          (actual (nth 1 (reverse (gethash 'apply bydi--history)))))
      (should (bydi-verify--was-called-with 'apply expected actual)))))

(ert-deftest bydi-was-called-last-with ()
  (bydi-match-expansion
   (bydi-was-called-last-with apply 'test)
   '(let ((expected (bydi-verify--safe-exp 'test))
          (actual (car-safe (last (reverse (gethash 'apply bydi--history))))))
      (should (bydi-verify--was-called-with 'apply expected actual)))))

(ert-deftest bydi-was-not-called ()
  (bydi-match-expansion
   (bydi-was-not-called apply)
   '(let ((actual (gethash 'apply bydi--history 'not-called)))
      (should (bydi-verify--was-not-called 'apply 'not-called actual)))))

(ert-deftest bydi-was-called-n-times ()
  (bydi-match-expansion

   (bydi-was-called-n-times apply 12)
   '(let ((actual (length (gethash 'apply bydi--history))))
      (should (bydi-verify--was-called-n-times 'apply 12 actual)))))

(ert-deftest bydi-match-expansion ()
  (bydi-match-expansion
   (bydi-match-expansion
    (should t))
   '(should
     (bydi-verify--matches
      '(should t)))))

(ert-deftest bydi-should-every ()
  (bydi-match-expansion
   (bydi-should-every (a b c) :check 'equal :expected 'test)
   '(progn
      (should ('equal a 'test))
      (should ('equal b 'test))
      (should ('equal c 'test)))))

(ert-deftest bydi-verify--safe-exp ()
  (should (equal (list nil) (bydi-verify--safe-exp nil)))
  (should (equal (list 'a) (bydi-verify--safe-exp 'a)))
  (should (equal (list 'a) (bydi-verify--safe-exp (list 'a)))))

(ert-deftest bydi-spy--spies ()
  (bydi ((:spy file-name-extension))
    (should (equal '("txt" "org" "el")
                   (mapcar #'file-name-extension '("one.txt" "two.org" "three.el"))))
    (bydi-was-called file-name-extension)
    (bydi-was-called-n-times file-name-extension 3)
    (bydi-was-called-nth-with file-name-extension "two.org" 1)))

(defvar watched nil)

(ert-deftest bydi-watch--watchers ()
  (bydi ((:watch watched))

    (setq watched 'a)
    (setq watched 'b)
    (setq watched 'c)

    (bydi-was-set-to watched 'c)
    (bydi-was-set-to-nth watched 'a 0)
    (bydi-was-set-to-nth watched 'b 1)
    (bydi-was-set-to-last watched 'c)
    (bydi-was-set-n-times watched 3)

    (setq watched nil)))

(ert-deftest bydi-watch--let-bindings ()
  (bydi ((:watch watched))

    (let ((watched 'let))
      (bydi-was-set-to watched 'let))))

(ert-deftest bydi-was-set ()
  (bydi ((:watch watched))

    (bydi-was-not-set watched)

    (setq watched 'set)

    (bydi-was-set watched)

    (setq watched nil)))

(ert-deftest bydi-was--clearing ()
  (bydi ((:watch watched))

    (setq watched 'set)

    (bydi-was-set-to watched 'set t)

    (bydi-was-not-set watched)

    (setq watched 'set)

    (bydi-was-set watched t)

    (bydi-was-not-set watched)))

(ert-deftest bydi-when--returns-conditionally ()
  (bydi ((:spy bydi-rf))

    (bydi-when bydi-rf :called-with '(2 2) :then-return 5 :once t)

    (should (equal 5 (bydi-rf 2 2)))
    (should (equal 2 (bydi-rf 2 2)))

    (bydi-when bydi-rf :called-with '(test this) :then-return 'what)

    (should (equal 'what (bydi-rf 'test 'this)))
    (should (equal 'test (bydi-rf 'test 'what)))
    (should (equal 'what (bydi-rf 'test 'this)))))

(ert-deftest bydi-when--warns ()
  (bydi bydi--warn
    (bydi-when bydi-rf :called-with '(test this) :then-return 'what)

    (bydi-was-called-with bydi--warn (list "No spy for `%s' was recorded" 'bydi-rf))))

(ert-deftest bydi-mock--fail ()
  :tags '(mock bindings)

  (bydi ((:fail previous-line)
         (:fail forward-line :with user-error)
         (:fail forward-paragraph :args '(no-catch "This is a test"))
         (:fail backward-paragraph :with user-error))

    (should-error (previous-line) :type 'error)
    (should-error (forward-line) :type 'user-error)
    (should-error (forward-paragraph) :type 'no-catch)
    (should-error (backward-paragraph) :type 'error)))

(ert-deftest bydi-mock--var ()
  :tags '(mock bindings)

  (bydi ((:mock previous-line :var test-var :initial "testing")
         (:risky-mock next-line :var mock-var))

    (should (string= (previous-line) "testing"))

    (setq test-var "mocking")

    (should (string= (previous-line) "mocking"))

    (should-not (next-line))

    (setq mock-var "testing")

    (should (string= (next-line) "testing"))))

;;; bydi-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
