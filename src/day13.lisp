(defpackage #:adventofcode2022/day13
  (:use #:cl #:adventofcode2022 #:binding-arrows))
(in-package #:adventofcode2022/day13)

(defun compare-lists (a b)
  (cond
    ((and (null a) (null b))
     :continue)
    ((null a)
     t)
    ((null b)
     nil)
    ((and (vectorp a) (not (vectorp b)))
     (compare-lists a (vector b)))
    ((and (not (vectorp a)) (vectorp b))
     (compare-lists (vector a) b))
    ((and (numberp a) (numberp b))
     (if (= a b)
         :continue
         (< a b)))
    (t (loop for ia from 0
             for ib from 0
             for va = (if (< ia (length a)) (aref a ia) nil)
             for vb = (if (< ib (length b)) (aref b ib) nil)
             for res = (compare-lists va vb)
             when (not (eq res :continue))
               return res
             end
             when (and (null va) (null vb))
               return :continue
             end
             finally (return t)))))

(defun task1 (inputs)
  (loop with inputs = (remove-if #'null inputs)
        for a = (pop inputs)
        for b = (pop inputs)
        for i from 1
        while a
        when (compare-lists a b)
          sum i))

(defun task2 (inputs)
  (let ((divider-packets (list #(#(2)) #(#(6)))))
    (apply #'*
           (loop for packet in (-> (remove-if #'null inputs)
                                 (append (copy-seq divider-packets))
                                 (sort (lambda (a b) (compare-lists a b))))
                 for i from 1
                 when (find packet divider-packets :test 'eq)
                   collect i))))

(define-day 13
    (:translate-input (lambda (line)
                        (if (= (length line) 0)
                            nil
                            (->> line
                              (str:replace-all "[" "#(")
                              (str:replace-all "]" ")")
                              (str:replace-all "," " ")
                              (read-from-string)))))
  #'task1
  #'task2)
