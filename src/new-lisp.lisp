(in-package :clts)

;; code to run in the new lisp image in the very first place
;; e.g.: quicklisp setup
(defparameter *prelude* '())

;;; place systems to load by the fresh lisp
;;; images to run the tests properly
(defparameter *systems-to-load* '())


(deftype nullable-stream () '(or stream null))
(deftype ext-prog-stream () '(or nullable-stream (eql :stream)))
(deftype synchronicity () '(member :sync :async))

(deftype exit-code () 'unsigned-byte)
(defparameter +exit-success+ 0)
(defparameter +exit-failure+ 1)
(defun exit-code-success? (ec) (zerop ec))
(defun exit-code-failure? (ec) (not (zerop ec)))

(defun run-in-new-lisp-image (program &key (sync :sync) out err)
  (declare (ftype (function (list &key
                                  (sync synchronicity)
                                  (out ext-prog-stream)
                                  (err ext-prog-stream))
                            exit-code)
                  run-in-new-lisp-image))
  #-sbcl (error "run-in-new-lisp-image: current implementation not supported")
  (let* ((func (ecase sync
                 (:sync (lambda (&rest args)
                          (nth-value 1 (apply #'external-program:run args))))
                 (:async #'external-program:start)))
         (tmp (cl-fad:open-temporary))
         (tmp-path (pathname tmp)))
    (format tmp "~{~S~%~}" program)
    (close tmp)
    (funcall func "sbcl" (list "--script" tmp-path)
             :output out :error err)))

(defun run-in-loaded-lisp-image (program &key (sync :sync) out err)
  (declare (ftype (function (list &key
                                  (sync synchronicity)
                                  (out ext-prog-stream)
                                  (err ext-prog-stream))
                            exit-code)
                  run-in-loaded-lisp-image))
  (let ((systems (mapcar (lambda (s-)
                           `(asdf:load-system ,s- :verbose nil))
                         (cons :clts *systems-to-load*))))
    (run-in-new-lisp-image (append *prelude*
                                   `((let ((*error-output* (make-broadcast-stream)))
                                       ,@systems))
                                   program)
                           :sync sync :out out :err err)))

;;; Macros

(defmacro with-new-lisp-image ((&optional (sync :sync) out err) &body program)
  (declare (type synchronicity sync))
  `(run-in-new-lisp-image ',program
                          :sync ,sync :out ,out :err ,err))

(defmacro with-loaded-lisp-image ((&optional (sync :sync) out err) &body program)
  (declare (type synchronicity sync))
  `(run-in-loaded-lisp-image ',program
                             :sync ,sync :out ,out :err ,err))
