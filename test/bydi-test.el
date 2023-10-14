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

(ert-deftest bydi-with-mock--simple ()
  (bydi-match-expansion
   (bydi-with-mock (bydi-rf bydi-ra)
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi-spy--spies 'nil)
         (bydi-watch--watchers 'nil)
         (bydi-mock--sometimes t)
         ((symbol-function 'bydi-rf)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'bydi-rf r))))
         ((symbol-function 'bydi-ra)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'bydi-ra r)))))
      (bydi--setup)
      (should (always))
      (bydi--teardown))))

(ert-deftest bydi-with-mock--old-usage ()
  (bydi-match-expansion
   (bydi-with-mock ((bydi-rt . #'ignore)
                    (format . (lambda (a &rest _args) a)))
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi-spy--spies 'nil)
         (bydi-watch--watchers 'nil)
         (bydi-mock--sometimes t)
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
      (bydi--setup)
      (should (always))
      (bydi--teardown))))

(ert-deftest bydi-with-mock--explicit ()
  (bydi-match-expansion
   (bydi-with-mock ((:return "hello" :mock substring)
                    (:risky-mock buffer-file-name :return "/tmp/test.el")
                    (:risky-mock bydi-ra :with ignore)
                    (:with always :mock buffer-live-p))
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi-spy--spies 'nil)
         (bydi-watch--watchers 'nil)
         (bydi-mock--sometimes t)
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
            (apply #'always r))))
      (bydi--setup)
      (should (always))
      (bydi--teardown))))

(ert-deftest bydi-with-mock--spies-and-watchers ()
  (bydi-match-expansion
   (bydi-with-mock ((:spy buffer-live-p)
                    (:mock abbrev-table-p :with bydi-rt)
                    (:spy derived-mode-p)
                    (:watch major-mode))
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi-spy--spies '(buffer-live-p derived-mode-p))
         (bydi-watch--watchers '(major-mode))
         (bydi-mock--sometimes t)
         ((symbol-function 'abbrev-table-p)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'abbrev-table-p r))
            (apply #'bydi-rt r))))
      (bydi--setup)
      (should (always))
      (bydi--teardown))))

(ert-deftest bydi-with-mock--shorthands ()
  (bydi-match-expansion
   (bydi-with-mock ((:ignore buffer-live-p)
                    (:always abbrev-table-p)
                    (:sometimes derived-mode-p))
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi-spy--spies 'nil)
         (bydi-watch--watchers 'nil)
         (bydi-mock--sometimes t)
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
            (funcall #'bydi-mock--sometimes))))
      (bydi--setup)
      (should (always))
      (bydi--teardown))))

(ert-deftest bydi-with-mock--single-function ()
  (bydi-match-expansion
   (bydi-with-mock bydi-rf
     (should (always)))
   `(cl-letf*
        ((bydi--history (make-hash-table :test 'equal))
         (bydi-spy--spies 'nil)
         (bydi-watch--watchers 'nil)
         (bydi-mock--sometimes t)
         ((symbol-function 'bydi-rf)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi--record (list 'bydi-rf r)))))
      (bydi--setup)
      (should (always))
      (bydi--teardown))))

(ert-deftest bydi-with-mock--unmockable ()
  (bydi-with-mock (bydi-mock--check)
    (let ((bydi-mock--risky '(new-line)))
      (bydi-match-expansion
       (bydi-with-mock ((:ignore new-line))
         nil)
       '(cl-letf*
            ((bydi--history
              (make-hash-table :test 'equal))
             (bydi-spy--spies 'nil)
             (bydi-watch--watchers 'nil)
             (bydi-mock--sometimes t)
             ((symbol-function 'new-line)
              (lambda
                (&rest r)
                (interactive)
                (apply 'bydi--record
                       (list 'new-line r))
                (apply #'ignore r))))
          (bydi--setup)
          nil
          (bydi--teardown))))
    (bydi-was-called bydi-mock--check)))

(ert-deftest bydi-with-mock--returning-nil ()
  (bydi-with-mock (bydi--warn)
    (bydi-match-expansion
     (bydi-with-mock ((:mock ignore :return nil))
       nil)
     '(cl-letf*
         ((bydi--history
           (make-hash-table :test 'equal))
          (bydi-spy--spies 'nil)
          (bydi-watch--watchers 'nil)
          (bydi-mock--sometimes t)
          ((symbol-function 'ignore)
           (lambda
             (&rest r)
             (interactive)
             (apply 'bydi--record
                    (list 'ignore r)))))
       (bydi--setup)
       nil
       (bydi--teardown)))

    (bydi-was-called-with bydi--warn "Returning 'nil' may lead to unexpected results")))

(ert-deftest bydi-clear-mocks ()
  (let ((bydi--history (make-hash-table :test 'equal)))

    (bydi--record 'test 'testing)

    (should (eq 1 (length (hash-table-keys bydi--history))))

    (bydi-clear-mocks)

    (should (eq 0 (length (hash-table-keys bydi--history))))))

(ert-deftest bydi-mock--check ()
  (bydi display-warning
    (let ((bydi-mock--risky '(ignore)))

      (bydi-mock--check 'ignore '(:mock ignore :return nil))
      (bydi-was-called-with display-warning (list 'bydi "Mocking 'ignore' may lead to issues" :warning))

      (bydi-clear-mocks)
      (bydi-mock--check 'ignore '(:risky-mock ignore :return nil))
      (bydi-was-not-called display-warning))))

(ert-deftest bydi-toggle-sometimes ()
  (bydi ((:sometimes buffer-live-p)
         (:othertimes hash-table-p)
         (:spy bydi-clear-mocks))

    (should (buffer-live-p))
    (should-not (hash-table-p))

    (bydi-toggle-sometimes)

    (should-not (buffer-live-p))
    (should (hash-table-p))

    (bydi-was-called bydi-clear-mocks)

    ;; Need to clear manually here.
    (setq bydi--history (make-hash-table :test 'equal))
    (bydi-toggle-sometimes t)

    (should (buffer-live-p))
    (bydi-was-not-called bydi-clear-mocks)))

(ert-deftest bydi-was-called ()
  (bydi-match-expansion
   (bydi-was-called apply)
   '(let ((actual (gethash 'apply bydi--history 'not-called)))
      (should (bydi-verify--was-called 'apply nil actual)))))

(ert-deftest bydi-was-called-with ()
  (bydi-match-expansion
   (bydi-was-called-with apply '(a b c))
   '(let ((actual (gethash 'apply bydi--history)))
      (should (bydi-verify--was-called-with 'apply '(a b c) (car actual))))))

(ert-deftest bydi-was-called-with--single-item ()
  (bydi-match-expansion
   (bydi-was-called-with apply "test")
   '(let ((actual (gethash 'apply bydi--history)))
      (should (bydi-verify--was-called-with 'apply "test" (car actual))))))

(ert-deftest bydi-was-called-with--partial-matching ()
  (let ((actual '(a b c d))
        (bydi-expect--elision '\...))

    (should (bydi-verify--was-called-with nil '(a b c d) actual))
    (should (bydi-verify--was-called-with nil '(... b c d) actual))
    (should (bydi-verify--was-called-with nil '(... b d) actual))
    (should (bydi-verify--was-called-with nil '(a ... d) actual))
    (should (bydi-verify--was-called-with nil '(... c) actual))
    (should-not (bydi-verify--was-called-with nil '(... b a) actual))))

(ert-deftest bydi-verify--was-called-with--nil-matching ()
  (should (bydi-verify--was-called-with nil nil nil)))

(ert-deftest bydi-was-called-nth-with ()
  (bydi-match-expansion
   (bydi-was-called-nth-with apply 'test 1)
   '(let ((actual (nth 1 (reverse (gethash 'apply bydi--history)))))
      (should (bydi-verify--was-called-with 'apply 'test actual)))))

(ert-deftest bydi-was-called-nth-with--single-item ()
  (bydi-match-expansion
   (bydi-was-called-nth-with apply "test" 1)
   '(let ((actual (nth 1 (reverse (gethash 'apply bydi--history)))))
      (should (bydi-verify--was-called-with 'apply "test" actual)))))

(ert-deftest bydi-was-called-last-with ()
  (bydi-match-expansion
   (bydi-was-called-last-with apply 'test)
   '(let ((actual (car-safe (last (reverse (gethash 'apply bydi--history))))))
      (should (bydi-verify--was-called-with 'apply 'test actual)))))

(ert-deftest bydi-was-not-called ()
  (bydi-match-expansion
   (bydi-was-not-called apply)
   '(let ((actual (gethash 'apply bydi--history 'not-called)))
      (should (bydi-verify--was-not-called 'apply nil actual)))))

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

;;; bydi-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
