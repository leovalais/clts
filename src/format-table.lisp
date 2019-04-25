(in-package :clts)

(defun %max-column-length (row-control table)
  (unless (every #'null table)
    (cons (apply #'max 0 (mapcar (lambda (row)
                                   (let ((cell (first row))
                                         (fmt (first row-control)))
                                     (if cell
                                         (length (format nil fmt cell))
                                         0)))
                                 table))
          (%max-column-length (rest row-control) (mapcar #'rest table)))))

(defun %format-row (stream row-control row column-lengths
                    cell-fmt empty-fmt eol-fmt)
  (let ((n-cells (loop :for n :from 0
                       :for cell :in row
                       :for max :in column-lengths
                       :for fmt :in row-control
                       :do (format stream cell-fmt (format nil fmt cell)
                                   (- max (length (format nil fmt cell))) '(" "))
                       :finally (return n))))
    (loop :for max :in (subseq column-lengths n-cells)
          :do (format stream empty-fmt max '(" ")))
    (format t eol-fmt)))

(defun %ensure-list-size (list size padding-element)
  (let ((n (length list)))
    (cond
      ((> n size) (subseq list 0 size))
      ((< n size) (append list (make-list (- size n) :initial-element padding-element)))
      (t list))))

(defun format-table (stream row-control table &key (borders t) heading)
  (destructuring-bind (cell-fmt empty-fmt eol-fmt)
      (if borders
          '("| ~a~v{~a~:*~} " "| ~v{~a~:*~} " "|~%")
          '("~a~v{~a~:*~} "   "~v{~a~:*~} "   "~%"))
    (let* ((n-columns (apply #'max 0 (mapcar #'length table)))
           (row-control (if (stringp row-control)
                            (make-list n-columns :initial-element row-control)
                            row-control))
           (heading (if (stringp heading)
                        (make-list n-columns :initial-element heading)
                        heading))
           (column-lengths (mapcar #'max
                                   (%max-column-length row-control table)
                                   (if heading
                                       (%ensure-list-size (%max-column-length heading
                                                                              (list (first table)))
                                                          n-columns 0)
                                       (make-list n-columns :initial-element 0))))
           (limit-vborder (apply #'concatenate 'string
                                 (append (mapcar (lambda (max)
                                                   (format nil "+-~v{~a~:*~}-" max '("-")))
                                                 column-lengths)
                                         (list "+~%")))))

      (when (zerop n-columns)
        (return-from format-table))

      (unless (and (= (length row-control) n-columns)
                   (or (null heading) (= (length heading) n-columns)))
        (error "row-control and table column number mismatch"))

      (when borders (format stream limit-vborder))

      (when heading
        (%format-row stream heading (first table) column-lengths cell-fmt empty-fmt eol-fmt)
        (when borders (format stream limit-vborder))
        (pop table))

      (dolist (row table)
        (%format-row stream row-control row column-lengths cell-fmt empty-fmt eol-fmt))

      (when borders (format stream limit-vborder)))))
