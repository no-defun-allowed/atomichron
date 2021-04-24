(defpackage :atomichron
  (:use :cl)
  (:export #:define-time-meter #:with-time-meter
           #:meters #:print-meters #:reset-meters))
