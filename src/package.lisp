(defpackage :clts
  (:use :cl :alexandria)
  (:export *test-suite*
           *token-separator-regexp*
           *timeout*
           *catch-errors*
           *process-exit?*
           *max-processes*
           *prelude*
           *systems-to-load*
           *printer-pass?*
           *printer-decorate?*
           *printer-metadata?*
           *validators*

           deftest
           defprinter

           add-test!
           delete-tests!
           add-deftest-validator!

           lets-go
           serial
           sexp-printer
           stat-printer
           individual-table-printer
           full-table-printer))
