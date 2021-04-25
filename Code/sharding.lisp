(in-package :atomichron)

(defvar *last-thread-id* 0)

(defun next-thread-id ()
  (loop for last = *last-thread-id*
        when (atomics:cas *last-thread-id* last (1+ last))
          return last))

(defconstant +shards+ 8
  "How many separate counters (\"shards\") to maintain to minimize contention.")
(defconstant +shard-spacing+ 16
  "How many elements each shard should use. Larger spacings minimize cache-line contention at the cost of using more memory.")
(defconstant +total-size+ (* +shards+ +shard-spacing+))

(defvar *thread-shard* 0)

(defun next-thread-shard ()
  (* +shard-spacing+ (mod (next-thread-id) +shards+)))

(push '(*thread-shard* . (next-thread-shard))
      bt:*default-special-bindings*)
