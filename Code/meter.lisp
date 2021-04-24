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
    `(let ((,meter-vector (make-incrementable-vector 2)))
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
        (declare (#-(and sbcl 64-bit) (simple-vector 2)
                    #+(and sbcl 64-bit) (simple-array sb-ext:word (2))
                    meter))
        (increment meter 0 1)
        (unless (zerop Δtime)
          ;; Avoid a CAS if we're not going to change the total time meter.
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

(defun print-meters (&key (groups '()) (stream *standard-output*))
  (when (null groups)
    (setf groups (sort (alexandria:hash-table-keys *meter-groups*)
                       #'string<)))
  (format stream "~&  ~32a ~8@a ~8@a ~8@a"
          "meter" "calls" "time" "latency")
  (dolist (group (alexandria:ensure-list groups))
    (format stream "~&Meters for ~a:" group)
    (dolist (information
             (sort (copy-list (meters group))
                   #'string< :key #'name))
      (let* ((calls    (aref (meter information) 0))
             (seconds  (seconds (aref (meter information) 1))))
        (format stream "~&  ~32a ~8,3e ~8,3e ~8,3e"
                (description information)
                calls seconds
                (/ seconds calls))))))

(defun reset-meters (groups)
  (when (null groups)
    (setf groups (alexandria:hash-table-keys *meter-groups*)))
  (dolist (group (alexandria:ensure-list groups))
    (dolist (information (meters group))
      (let ((meter (meter information)))
        (fill meter 0)))))
