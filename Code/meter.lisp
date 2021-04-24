(in-package :atomichron)

;;; This lock is only ever acquired in DEFINE-TIME-METER - metering
;;; itself is wait-free.
(defvar *meter-group-lock* (bt:make-lock "Meter group lock"))
(defvar *meter-groups* (make-hash-table :test 'equalp))

(defmacro defglobal (name initial-value)
  #+sbcl `(sb-ext:defglobal ,name ,initial-value)
  #+ccl `(ccl:defstaticvar ,name ,initial-value)
  #-(or ccl sbcl) `(defvar ,name ,initial-value))

(defstruct (meter-information (:conc-name ""))
  (meter)
  (description)
  (name))

#-disable-meters
(defmacro define-time-meter (name description group)
  (check-type name symbol)
  (check-type description string) 
  (alexandria:with-gensyms (meter-vector)
    `(let ((,meter-vector
             #-(and sbcl 64-bit) (make-array 3 :initial-element 0)
             #+(and sbcl 64-bit) (make-array 3 :initial-element 0
                                               :element-type 'sb-ext:word)))
       (defglobal ,name ,meter-vector)
       (bt:with-lock-held (*meter-group-lock*)
         (pushnew (make-meter-information
                   :name ',name
                   :description ,description
                   :meter ,meter-vector)
                  (gethash ',group *meter-groups*)
                  :key #'name))
       ',name)))

#+disable-meters
(defmacro define-time-meter (name description group)
  (declare (ignore name description group))
  `(progn))

(defmacro increment (meter index Δ)
  ;; We could use ATOMICS:ATOMIC-INCF, but that could overflow on 32-bit hosts
  ;; and is generally a bit more annoying.
  #-(and sbcl 64-bit)
  `(loop for last-observed = (svref ,meter ,index)
         until (atomics:cas (svref ,meter ,index)
                            last-observed (+ last-observed ,Δ)))
  #+(and sbcl 64-bit)
  `(atomics:atomic-incf (aref ,meter ,index) ,Δ))

#-disable-meters
(defmacro with-time-meter ((name) &body body)
  (alexandria:with-gensyms (meter-vector start-time end-time Δtime)
    `(let ((,meter-vector ,name)
           (,start-time (get-internal-real-time)))
       (unwind-protect
            (progn ,@body)
         (let* ((,end-time (get-internal-real-time))
                (,Δtime    (- ,end-time ,start-time)))
           (declare (fixnum ,Δtime)
                    (#-(and sbcl 64-bit) (simple-vector 3)
                     #+(and sbcl 64-bit) (simple-array sb-ext:word (3))
                       ,meter-vector))
           (increment ,meter-vector 0 1)
           (unless (zerop ,Δtime)
             ;; Avoid a CAS if we're not going to change the total time meter.
             (increment ,meter-vector 2 (expt ,Δtime 2))
             (increment ,meter-vector 1 ,Δtime)))))))

#+disable-meters
(defmacro with-time-meter ((name) &body body)
  (declare (ignore name))
  `(progn ,@body))
           
(defun meters (group)
  (values (gethash group *meter-groups*)))
(defun seconds (internal-time-units)
  (float (/ internal-time-units internal-time-units-per-second)))

(defun print-meters (groups &key (stream *standard-output*))
  (when (null groups)
    (setf groups (sort (alexandria:hash-table-keys *meter-groups*)
                       #'string<)))
  (format stream "~&  ~32a ~8@a ~8@a ~8@a ~8@a"
          "meter" "calls" "time" "latency" "stdev")
  (dolist (group (alexandria:ensure-list groups))
    (format stream "~&Meters for ~a:" group)
    (dolist (information
             (sort (copy-list (meters group))
                   #'string< :key #'name))
      (let* ((calls    (aref (meter information) 0))
             (seconds  (seconds (aref (meter information) 1)))
             ;; Suppose our time T has been multiplied by
             ;; internal-time-units-per-second to produce U,
             ;; i.e. U = i-t-u-p-s × T; T = U / i-t-u-p-s
             ;; Then T² = (U / i-t-u-p-s)² = U² / i-t-u-p-s²
             (seconds² (seconds (seconds (aref (meter information) 2)))))
        (if (zerop calls)
            (format stream "~&  ~32a ~8,3e ~8,3e      n/a      n/a"
                    (description information) calls seconds)
            (let ((standard-deviation
                    (sqrt (- (/ seconds² calls) (expt (/ seconds calls) 2)))))
              (format stream "~&  ~32a ~8,3e ~8,3e ~8,3e ~8,3e"
                      (description information)
                      calls seconds
                      (/ seconds calls)
                      standard-deviation)))))))

(defun reset-meters (groups)
  (when (null groups)
    (setf groups (alexandria:hash-table-keys *meter-groups*)))
  (dolist (group (alexandria:ensure-list groups))
    (dolist (pair (meters group))
      (setf (aref (cdr pair) 0) 0
            (aref (cdr pair) 1) 0
            (aref (cdr pair) 2) 0))))
