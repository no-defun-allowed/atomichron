(atomichron:define-time-meter *test-body* "Literally nothing" benchmark)
(atomichron:define-time-meter *test* "Metering overhead ×1.0e8" benchmark)

(defun run-work ()
  (atomichron:with-time-meter (*test*)
    (dotimes (i 100000000)
      (atomichron:with-time-meter (*test-body*)))))

(defun run-test (&key (thread-count 4))
  (atomichron:reset-meters '(benchmark))
  (loop repeat thread-count
        collect (bt:make-thread #'run-work) into threads
        finally (mapc #'bt:join-thread threads))
  (atomichron:print-meters '(benchmark)))
