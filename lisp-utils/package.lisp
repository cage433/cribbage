(in-package :common-lisp-user)

(defpackage :cage433-lisp-utils
  (:use :common-lisp)
  (:export :deftest
           :load-and-compile-if-necessary
           :combine-results
           :===
           :dbind
           :check
           :with-gensyms
           ))
