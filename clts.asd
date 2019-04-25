;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :clts
  :name "clts"
  :version "0.1"
  :maintainer "Léo Valais"
  :author "Léo Valais"
  :licence "MIT"
  :description "Isolated, generic test-suite processor compatible with P.Dietz deftest interface."

  :depends-on (:alexandria
               :cl-ppcre
               :external-program
               :trivial-timeout
               :cl-fad)

  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "format-table")
                             (:file "new-lisp")
                             (:file "clts")))))
