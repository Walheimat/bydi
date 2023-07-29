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
        ((bydi-mock-history (make-hash-table :test 'equal))
         (bydi-mock-sometimes t)
         (bydi-spies 'nil)
         ((symbol-function 'bydi-rf)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'bydi-rf r))))
         ((symbol-function 'bydi-ra)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'bydi-ra r)))))
      (bydi-spy--create)
      (should (always))
      (bydi-spy--clear))))

(ert-deftest bydi-with-mock--old-usage ()
  (bydi-match-expansion
   (bydi-with-mock ((bydi-rt . #'ignore)
                    (format . (lambda (a &rest _args) a)))
     (should (always)))
   `(cl-letf*
        ((bydi-mock-history (make-hash-table :test 'equal))
         (bydi-mock-sometimes t)
         (bydi-spies 'nil)
         ((symbol-function 'bydi-rt)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'bydi-rt r))
            (apply #'ignore r)))
         ((symbol-function 'format)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'format r))
            (apply (lambda (a &rest _args) a) r))))
      (bydi-spy--create)
      (should (always))
      (bydi-spy--clear))))

(ert-deftest bydi-with-mock--explicit ()
  (bydi-match-expansion
   (bydi-with-mock ((:return "hello" :mock substring)
                    (:mock buffer-file-name :return "/tmp/test.el")
                    (:mock bydi-ra :with ignore)
                    (:with always :mock buffer-live-p))
     (should (always)))
   `(cl-letf*
        ((bydi-mock-history (make-hash-table :test 'equal))
         (bydi-mock-sometimes t)
         (bydi-spies 'nil)
         ((symbol-function 'substring)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'substring r))
            "hello"))
         ((symbol-function 'buffer-file-name)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'buffer-file-name r))
            "/tmp/test.el"))
         ((symbol-function 'bydi-ra)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'bydi-ra r))
            (apply #'ignore r)))
         ((symbol-function 'buffer-live-p)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'buffer-live-p r))
            (apply #'always r))))
      (bydi-spy--create)
      (should (always))
      (bydi-spy--clear))))

(ert-deftest bydi-with-mock--spies ()
  (bydi-match-expansion
   (bydi-with-mock ((:spy buffer-live-p)
                    (:mock abbrev-table-p :with bydi-rt)
                    (:spy derived-mode-p))
     (should (always)))
   `(cl-letf*
        ((bydi-mock-history (make-hash-table :test 'equal))
         (bydi-mock-sometimes t)
         (bydi-spies '(buffer-live-p derived-mode-p))
         ((symbol-function 'abbrev-table-p)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'abbrev-table-p r))
            (apply #'bydi-rt r))))
      (bydi-spy--create)
      (should (always))
      (bydi-spy--clear))))

(ert-deftest bydi-with-mock--shorthands ()
  (bydi-match-expansion
   (bydi-with-mock ((:ignore buffer-live-p)
                    (:always abbrev-table-p)
                    (:sometimes derived-mode-p))
     (should (always)))
   `(cl-letf*
        ((bydi-mock-history (make-hash-table :test 'equal))
         (bydi-mock-sometimes t)
         (bydi-spies 'nil)
         ((symbol-function 'buffer-live-p)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'buffer-live-p r))
            (apply #'ignore r)))
         ((symbol-function 'abbrev-table-p)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'abbrev-table-p r))
            (apply #'always r)))
         ((symbol-function 'derived-mode-p)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'derived-mode-p r))
            (funcall #'bydi-mock--sometimes))))
      (bydi-spy--create)
      (should (always))
      (bydi-spy--clear))))

(ert-deftest bydi-with-mock--single-function ()
  (bydi-match-expansion
   (bydi-with-mock bydi-rf
     (should (always)))
   `(cl-letf*
        ((bydi-mock-history (make-hash-table :test 'equal))
         (bydi-mock-sometimes t)
         (bydi-spies 'nil)
         ((symbol-function 'bydi-rf)
          (lambda (&rest r)
            (interactive)
            (apply 'bydi-mock--remember (list 'bydi-rf r)))))
      (bydi-spy--create)
      (should (always))
      (bydi-spy--clear))))

(ert-deftest bydi-with-mock--unmockable ()
  (bydi-with-mock (bydi-warn--bad-mock)
    (let ((bydi--never-mock '(new-line)))
      (bydi-match-expansion
       (bydi-with-mock ((:ignore new-line))
         nil)
       '(cl-letf*
            ((bydi-mock-history
              (make-hash-table :test 'equal))
             (bydi-mock-sometimes t)
             (bydi-spies 'nil)
             ((symbol-function 'new-line)
              (lambda
                (&rest r)
                (interactive)
                (apply 'bydi-mock--remember
                       (list 'new-line r))
                (apply #'ignore r))))
          (bydi-spy--create)
          nil
          (bydi-spy--clear))))
    (bydi-was-called bydi-warn--bad-mock)))

(ert-deftest bydi-clear-mocks ()
  (let ((bydi-mock-history nil))
    (bydi-clear-mocks)
    (should bydi-mock-history)))

(ert-deftest bydi-toggle-sometimes ()
  (bydi ((:sometimes buffer-live-p)
         (:spy bydi-clear-mocks))

    (should (buffer-live-p))

    (bydi-toggle-sometimes)

    (should-not (buffer-live-p))
    (bydi-was-called bydi-clear-mocks)

    ;; Need to clear manually here.
    (setq bydi-mock-history (make-hash-table :test 'equal))
    (bydi-toggle-sometimes t)

    (should (buffer-live-p))
    (bydi-was-not-called bydi-clear-mocks)))

(ert-deftest bydi-was-called ()
  (bydi-match-expansion
   (bydi-was-called apply)
   '(let ((actual (gethash 'apply bydi-mock-history 'not-called)))
      (should (bydi--was-called 'apply nil actual)))))

(ert-deftest bydi-was-called-with ()
  (bydi-match-expansion
   (bydi-was-called-with apply '(a b c))
   '(let ((actual (gethash 'apply bydi-mock-history)))
      (should (bydi--was-called-with 'apply '(a b c) (car actual))))))

(ert-deftest bydi-was-called-with--single-item ()
  (bydi-match-expansion
   (bydi-was-called-with apply "test")
   '(let ((actual (gethash 'apply bydi-mock-history)))
      (should (bydi--was-called-with 'apply (list "test") (car actual))))))

(ert-deftest bydi-was-called-with--partial-matching ()
  (let ((actual '(a b c d))
        (bydi--elision '\...))

    (should (bydi--was-called-with nil '(a b c d) actual))
    (should (bydi--was-called-with nil '(... b c d) actual))
    (should (bydi--was-called-with nil '(... b d) actual))
    (should (bydi--was-called-with nil '(a ... d) actual))
    (should (bydi--was-called-with nil '(... c) actual))
    (should-not (bydi--was-called-with nil '(... b a) actual))))

(ert-deftest bydi-was-called-nth-with ()
  (bydi-match-expansion
   (bydi-was-called-nth-with apply 'test 1)
   '(let ((actual (nth 1 (reverse (gethash 'apply bydi-mock-history)))))
      (should (bydi--was-called-with 'apply 'test actual)))))

(ert-deftest bydi-was-called-nth-with--single-item ()
  (bydi-match-expansion
   (bydi-was-called-nth-with apply "test" 1)
   '(let ((actual (nth 1 (reverse (gethash 'apply bydi-mock-history)))))
      (should (bydi--was-called-with 'apply (list "test") actual)))))

(ert-deftest bydi-was-called-last-with ()
  (bydi-match-expansion
   (bydi-was-called-last-with apply 'test)
   '(let ((actual (car-safe (last (reverse (gethash 'apply bydi-mock-history))))))
      (should (bydi--was-called-with 'apply 'test actual)))))

(ert-deftest bydi-was-not-called ()
  (bydi-match-expansion
   (bydi-was-not-called apply)
   '(let ((actual (gethash 'apply bydi-mock-history 'not-called)))
      (should (bydi--was-not-called 'apply nil actual)))))

(ert-deftest bydi-was-called-n-times ()
  (bydi-match-expansion

   (bydi-was-called-n-times apply 12)
   '(let ((actual (length (gethash 'apply bydi-mock-history))))
      (should (bydi--was-called-n-times 'apply 12 actual)))))

(ert-deftest bydi-match-expansion ()
  (bydi-match-expansion
   (bydi-match-expansion
    (should t))
   '(should
    (bydi-match-expansion--matches
     '(should t)))))

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

(ert-deftest bydi-path-setup ()
  (bydi (bydi-ci--setup-paths)

    (bydi-path-setup (list "test" "mock"))

    (bydi-was-called bydi-ci--setup-paths)))

(ert-deftest bydi--matches-in-string ()
  (let ((str "This 1 string has 3 matches, or is it 2?")
        (pattern "\\(?1:[[:digit:]]\\)"))

    (should (equal '("2" "3" "1") (bydi--matches-in-string pattern str)))))

(ert-deftest bydi-spy--spies ()
  (bydi ((:spy file-name-extension))
    (should (equal '("txt" "org" "el")
                   (mapcar #'file-name-extension '("one.txt" "two.org" "three.el"))))
    (bydi-was-called file-name-extension)
    (bydi-was-called-n-times file-name-extension 3)
    (bydi-was-called-nth-with file-name-extension "two.org" 1)))

(ert-deftest bydi--warn ()
  (bydi display-warning
    (bydi-warn--bad-mock 'ignore)
    (bydi-was-called-with display-warning (list 'bydi "Mocking ignore may lead to issues" :warning))))

;;; bydi-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
