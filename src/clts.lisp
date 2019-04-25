(in-package :clts)

(defparameter *test-suite* '())
(defparameter *token-separator-regexp* "\\.")
(defparameter *timeout* 5)
(defparameter *catch-errors* t)
(defparameter *process-exit?* nil)

(defclass metadata ()
  ((expected-results :type list
                     :accessor expected-results
                     :initarg :expected-results
                     :initform '())
   (code :type list
         :accessor code
         :initarg :code
         :initform '()))
  (:documentation "Metadata for the tests. Only matters for display and debbuging purposes."))

(defclass test ()
  ((name :type symbol
         :initarg :name
         :reader name
         :initform (error "must supply test name"))
   (func :type (function () t)
         :initarg :func
         :reader func
         :initform (error "must supply test function"))
   (validator :type (function (t) boolean)
              :initarg :validator
              :reader validator
              :initform (constantly nil))
   (metadata :type metadata
             :accessor metadata
             :initarg :metadata
             :initform (make-instance 'metadata)))
  (:documentation "A test."))

(defun add-test! (test)
  (declare (ftype (function (test) nil) add-test!))
  (when-let (t- (find-test (name test)))
    (delete-test! t-))
  (push test *test-suite*)
  (values))

(defun tokenize (symbol-token)
  (mapcar #'symbolicate
          (cl-ppcre:split *token-separator-regexp*
                          (symbol-name symbol-token))))

(defun collect-tokens (tokens-or-symbol-tokens)
  (remove-duplicates (mappend #'tokenize tokens-or-symbol-tokens)))

(defun name-match? (name tokens)
  (null (set-difference (tokenize name)
                        tokens)))

(defun tests-matching (tokens)
  (if tokens
      (remove-if-not (lambda (n-) (name-match? n- tokens))
                     *test-suite*
                     :key #'name)
      *test-suite*))

(defun find-test (name)
  (declare (ftype (function (symbol) (or test null))
                  find-test))
  (find name *test-suite* :key #'name))

(defun delete-test! (test)
  (declare (ftype (function (test) nil) delete-test!))
  (setf *test-suite* (remove test *test-suite*))
  (values))

(defun delete-tests! (&rest tokens-or-symbol-tokens)
  (declare (ftype (function (&rest symbol) list) delete-tests!))
  (let* ((tokens (collect-tokens tokens-or-symbol-tokens))
         (tests (tests-matching tokens)))
    (mapc #'delete-test! tests)
    tests))



(deftype test-status--pass () '(eql :pass))
(deftype test-status--fail () '(eql :fail))
(deftype test-status--cond () '(cons (eql :cond) symbol))
(deftype test-status () '(or test-status--pass test-status--fail test-status--cond))
(defun make-pass () (declare (ftype (function () test-status--pass) make-pass)) :pass)
(defun make-fail () (declare (ftype (function () test-status--fail) make-fail)) :fail)
(defun make-cond (c)
  (declare (ftype (function () test-status--cond) make-cond))
  (cons :cond (class-name (class-of c))))
(defun pass? (x) (declare (ftype (function (t) boolean) pass?)) (typep x 'test-status--pass))
(defun fail? (x) (declare (ftype (function (t) boolean) fail?)) (typep x 'test-status--fail))
(defun cond? (x) (declare (ftype (function (t) boolean) cond?)) (typep x 'test-status--cond))
(defun status-indicator (s)
  (declare (ftype (function (test-status) (member :pass :fail :cond)) status-indicator))
  (cond ((pass? s) :pass)
        ((fail? s) :fail)
        ((cond? s) :cond)))
(defun status->string (s)
  (declare (ftype (function (test-status) string) status->string))
  (cond
    ((pass? s) "PASS")
    ((fail? s) "FAIL")
    ((cond? s) (symbol-name (cdr s)))))


(defstruct (report (:print-object write-report))
  (name (error "must supply name") :type symbol)
  (status (error "must supply status") :type test-status)
  (results :no-results :type t))
(deftype report-list () 'list)
(defun write-report (report stream)
  (declare (ftype (function (report stream) nil) write-report))
  (flet ((readable? (thing)
           (handler-case
               (progn
                 (with-input-from-string (in (with-output-to-string (out)
                                               (write thing :stream out :readably t)))
                   (read in))
                 t)
             (print-not-readable (e)
               (declare (ignore e))
               nil))))
    (write (list :name (report-name report)
                 :status (report-status report)
                 :results (if (readable? (report-results report))
                              (report-results report)
                              '++!NOT-PRINTABLE!++))
           :stream stream :readably t)))
(defun plist->report (plist)
  (declare (ftype (function (list) report) plist->report))
  (flet ((getf-or-die (indicator)
           (or (getf plist indicator)
               (error "no report for such plist: ~S" plist))))
    (make-report :name (getf-or-die :name)
                 :status (getf-or-die :status)
                 :results (getf plist :results))))


(defun funcall-test (test)
  (declare (type test test))
  (trivial-timeout:with-timeout (*timeout*)
    (multiple-value-list (funcall (let ((*error-output* *standard-output*))
                                    ;; We redirect stderr to stdin because
                                    ;; a test's error is a test suite's output.
                                    (func test))))))

(defun eval-test (test)
  (declare (type test test))
  (if *catch-errors*
      (handler-case (funcall-test test)
        (condition (e) e))
      (funcall-test test)))

(defun validate-test (test results)
  (declare (ftype (function (test t) boolean) validate-test))
  (apply (validator test) results))

(defun run-test (test)
  (declare (ftype (function (test) report) run-test))
  (let* ((results (eval-test test))
         (status (cond
                   ((typep results 'condition) (make-cond results))
                   ((validate-test test results) (make-pass))
                   (t (make-fail)))))
    (make-report :name (name test)
                 :results results
                 :status status)))

(defun run-test-suite (tokens report-stream)
  (declare (ftype (function (list nullable-stream) report-list) run-test-suite))
  (flet ((do-test (test)
           (let ((report (run-test test)))
             report)))
    (let* ((tests (tests-matching (collect-tokens tokens)))
           (reports (mapcar #'do-test tests)))
      (when report-stream
        (write reports :stream report-stream))
      reports)))

(defun exit-with-code! (exit-code)
  (declare (ftype (function (exit-code) exit-code) exit-with-code!))
  (when *process-exit?*
    (sb-ext:exit :code exit-code))
  exit-code)

(defun exit-with-reports! (reports)
  (declare (ftype (function (report stream) exit-code) exit-with-reports!))
  (let ((ex-code (if (find-if (lambda (s-) (not (pass? s-))) reports
                              :key #'report-status)
                     +exit-failure+
                     +exit-success+)))
    (exit-with-code! ex-code)))

(defparameter *max-processes* 8)
(defun pool-map-to-processes (function list)
  (let ((processes '()))
    (flet ((can-go? ()
             (< (count-if (lambda (p)
                            (not (eql (external-program:process-status p) :exited)))
                          processes)
                *max-processes*))
           (all-exited? ()
             (every (lambda (p)
                      (eql (external-program:process-status p) :exited))
                    processes)))
      (loop
        :for x :in list
        :do (loop :until (can-go?))
        :do (push (funcall function x) processes)
        :finally (loop :until (all-exited?))))
    processes))

(defun read-all-stream (stream &optional stop-flag-value (test #'eql))
  (declare (ftype (function (stream &optional t (function (t t) boolean))) read-all-stream))
  (let ((x (read stream nil stop-flag-value)))
    (if (funcall test x stop-flag-value)
        '()
        (cons x (read-all-stream stream stop-flag-value test)))))

;;; FIXME some errors occasionally arise but I haven't found a consistent pattern.
;;; Fortunately these does not seem to happen in the first `lets-go' call so I
;;; guess it's ok to use it in a CI. Race-condition with streams perhaps?
(defun run (tokens report-stream &key (fresh-lisp :none) (sync :sync))
  (declare (ftype (function (list nullable-stream &key
                                  (fresh-lisp (member :none :one :one-per-test))
                                  (sync synchronicity))
                            exit-code)
                  run))
  (ecase fresh-lisp
    (:one
     (exit-with-code!
      (run-in-loaded-lisp-image
       `((in-package :clts)
         (let ((reports (run-test-suite ',tokens *standard-output*))
               (*process-exit?* t))
           (exit-with-reports! reports)))
       ;; Since `run' ends only when every test finishes, it doesn't make sense
       ;; to run that single lisp image in background (`:async') and then
       ;; immediately wait for it to return: just use `:sync' instead.
       :sync :sync :out report-stream :err *error-output*)))
    (:one-per-test
     (flet ((run-test-in-fresh-lisp (test &optional (stdout report-stream))
              (run-in-loaded-lisp-image
               `((in-package :clts)
                 (let ((reports (run-test-suite ',(list (name test)) *standard-output*))
                       (*process-exit?* t))
                   (exit-with-reports! reports)))
               :sync sync :out stdout :err *error-output*))
            (finish (exit-codes)
              (exit-with-code! (if (some (complement #'exit-code-success?) exit-codes)
                                   +exit-failure+
                                   +exit-success+))))
       (let ((tests (tests-matching (collect-tokens tokens))))
         (flet ((exec-async ()
                  ;; For some reason, when redirecting `report-stream' to `external-program's
                  ;; `:output', nothing gets written. The workaround uses the ability of
                  ;; `start' (background process) to create a new stream when the value of
                  ;; `:output' is `:stream'. So each process has its own output stream. When
                  ;; every test is finished, all the output streams created are read at once
                  ;; and the data is finally written on `report-stream'. ¯\_(ツ)_/¯
                  ;; -----
                  ;; The reports can appear in any order (not necessarily the one in which tests
                  ;; have completed) but that's ok because since the processes are asynchronous
                  ;; there is no way to guarantee a specific ordering. Anyway this should be
                  ;; done by the printer since the `report-list' has conceptually more of
                  ;; a set than a list...
                  (let* ((processes (pool-map-to-processes (lambda (t-)
                                                             (run-test-in-fresh-lisp t- :stream))
                                                           tests))
                         (exit-codes (mapcar (lambda (p-)
                                               (nth-value 1 (external-program:process-status p-)))
                                             processes)))
                    (format report-stream "~{~S~}"
                            (mappend (lambda (p-)
                                       (read-all-stream (external-program:process-output-stream p-)))
                                     processes))
                    exit-codes))
                (exec-sync ()
                  (mapcar #'run-test-in-fresh-lisp tests)))
           (finish (if (eql sync :async)
                       (exec-async)
                       (exec-sync)))))))
    (:none
     (exit-with-reports!
      (run-test-suite tokens report-stream)))))

(defun lets-go (printer &key tests (fresh-lisp :none) (sync :sync))
  (when (and (symbolp tests) (not (null tests))) (setf tests (list tests)))
  (let* ((exit-code +exit-failure+)
         (out-string (with-output-to-string (out)
                       (setf exit-code (run tests out :fresh-lisp fresh-lisp :sync sync))))
         (data (with-input-from-string (in out-string)
                 (read-all-stream in)))
         (reports (mappend (lambda (report-list)
                             (mapcar #'plist->report report-list))
                           data)))
    (funcall printer reports)
    exit-code))



(defparameter *printer-pass?* nil)
(defparameter *printer-decorate?* t)
(defparameter *printer-metadata?* t) ; UNUSED

(defmacro defprinter (name (reports &rest key-args) &body body)
  `(defun ,name (&key (stream *standard-output*)
                      (pass? *printer-pass?*)
                      (decorate? *printer-decorate?*)
                      (metadata? *printer-metadata?*)
                   ,@key-args)
     (lambda (,reports)
       (let ((*printer-pass?* pass?)
             (*printer-decorate?* decorate?)
             (*printer-metadata?* metadata?)
             (*printer-stream* stream))
         (declare (special *printer-stream*))
         ,@body))))

(defun serial (&rest printers)
  (lambda (reports)
    (values-list (mapcar (lambda (p-) (funcall p- reports))
                         printers))))

(defprinter sexp-printer (reports (pretty? t))
            (write reports :stream *printer-stream* :readably t :pretty pretty?))

(defprinter stat-printer (reports)
            (flet ((count-status (pred)
                     (count-if pred reports :key #'report-status)))
              (let* ((pass (count-status #'pass?))
                     (fail (count-status #'fail?))
                     (cond- (count-status #'cond?))
                     (total (+ pass fail cond-)))
                (format-table *printer-stream* "~a"
                              (list (list "Pass" pass)
                                    (list "Fail" fail)
                                    (list "Cond" cond-)
                                    (list "Total" total)
                                    (list "Ratio" (if (zerop total)
                                                      1
                                                      (/ pass total))))
                              :borders *printer-decorate?*))))

(defprinter individual-table-printer (reports)
            (dolist (r- reports)
              (print-report r- *printer-stream*)))

(defprinter full-table-printer (reports)
            (print-full-report reports *printer-stream*))

;; TODO keep track of the metadata of the corresponding test and print then
;; when `*print-metadata*'.
(defun print-report (report out)
  (unless (and (not *printer-pass?*)
               (pass? (report-status report)))
    (format-table out "~a"
                  (list (list "Name" (symbol-name (report-name report)))
                        (list "Results" (format nil "~a" (report-results report)))
                        (list "Status" (status->string (report-status report))))
                  :borders *printer-decorate?*)))

(defun print-full-report (reports out)
  (flet ((report->table-data (report)
           (list (symbol-name (report-name report))
                 (status->string (report-status report)))))
    (unless *printer-pass?*
      (setf reports (remove-if (lambda (r-) (pass? (report-status r-))) reports)))
    (let ((table-data (mapcar #'report->table-data reports)))
      (when *printer-decorate?*
        (setf table-data (cons '("Test name" "Status") table-data)))
      (format-table out "~a" table-data
                    :borders *printer-decorate?*
                    :heading (and *printer-decorate?* "~a")))))


;;; Macros

(defparameter *validators*
  '((number . =)
    (string . string=)
    (character . char=)
    (sequence . equal)
    (pathname . equal)
    (t . eql)))

(defun add-deftest-validator! (type validator)
  (push (list type validator) *validators*)
  (values))

(defun validator-of-value (value)
  (let ((fval (cdr (find-if (lambda (v-)
                              (typep value (car v-)))
                            *validators*))))
    (etypecase fval
      (function fval)
      ((and symbol
            (not keyword)
            (not null))
       (symbol-function fval))
      (null (error "no validator found for ~S" value)))))

(defun make-validator (value)
  (let ((value= (validator-of-value value)))
    (lambda (x) (funcall value= x value))))

(defun make-multi-validator (values)
  (let ((validators (mapcar #'make-validator values)))
    (flet ((zip (&rest lists)
             (apply #'mapcar #'list lists)))
      (lambda (&rest results)
        (every (lambda (args)
                 (destructuring-bind (validator res) args
                   (funcall validator res)))
               (zip validators results))))))


(defmacro deftest (name &body (body &rest expected-results))
  (let ((validator (if expected-results
                       `(make-multi-validator ',expected-results)
                       '(symbol-function 'identity))))
    `(add-test! (make-instance 'test
                               :name ',name
                               :func (lambda () ,body)
                               :validator ,validator
                               :metadata (make-instance 'metadata
                                                        :expected-results ',expected-results
                                                        :code ',body)))))
