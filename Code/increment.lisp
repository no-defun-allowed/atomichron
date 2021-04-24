(in-package :atomichron)

(defmacro increment (meter index Δ)
  #-(and (or ccl sbcl) 64-bit)
  `(loop for last-observed = (svref ,meter ,index)
         until (atomics:cas (svref ,meter ,index)
                            last-observed (+ last-observed ,Δ)))
  #+(and sbcl 64-bit)
  `(atomics:atomic-incf (aref ,meter ,index) ,Δ)
  #+(and ccl 64-bit-host)
  `(atomics:atomic-incf (svref ,meter ,index) ,Δ))

(defun make-incrementable-vector (length)
  #-(and sbcl 64-bit)
  (make-array length
              :initial-element 0)
  #+(and sbcl 64-bit)
  (make-array length
              :initial-element 0
              :element-type 'sb-ext:word))
