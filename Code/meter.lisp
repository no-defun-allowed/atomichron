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
  #-(and (or ccl sbcl) 64-bit)
  `(loop for last-observed = (svref ,meter ,index)
         until (atomics:cas (svref ,meter ,index)
                            last-observed (+ last-observed ,Δ)))
  #+(and sbcl 64-bit)
  `(atomics:atomic-incf (aref ,meter ,index) ,Δ)
  #+(and ccl 64-bit-host)
  `(atomics:atomic-incf (svref ,meter ,index) ,Δ))

(declaim (inline call-with-time-meter))
#-disable-meters
(defun call-with-time-meter (meter continuation)
  (declare (optimize (speed 3))
           (function continuation)
           #+sbcl
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((start-time (get-internal-real-time)))
    (unwind-protect
         (funcall continuation)
      (let* ((end-time (get-internal-real-time))
             (Δtime    (- end-time start-time)))
        (declare (#-(and sbcl 64-bit) (simple-vector 3)
                    #+(and sbcl 64-bit) (simple-array sb-ext:word (3))
                    meter))
        (increment meter 0 1)
        (unless (zerop Δtime)
          ;; Avoid a CAS if we're not going to change the total time meter.
          (increment meter 2 (expt Δtime 2))
          (increment meter 1 Δtime))))))

#+disable-meters
(defun call-with-time-meter (meter continuation)
  (declare (ignore meter))
  (funcall continuation))

(defmacro with-time-meter ((name) &body body)
  `(call-with-time-meter ,name (lambda () ,@body)))
           
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
    (dolist (information (meters group))
      (let ((meter (meter information)))
        (fill meter 0)))))
