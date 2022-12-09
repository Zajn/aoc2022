;; Advent of Code 2022
;; Day 1
;; Input: List of calories carried by each elf. Newline separates previous elf's inventory from current
;; Find elf carrying most calories, and how many calories that elf carries
(defpackage :advent-of-code
  (:use :cl)
  (:export
   #:calorie-count
   #:calorie-count-top-3))

(in-package :advent-of-code)

(defun calorie-count (list-file)
  (let ((maximum 0)
        (current 0))
    (with-open-file (input list-file)
      (loop for line = (read-line input nil)
            while line do
               (if (= (length line) 0)
                   (setf maximum (max maximum current)
                         current 0)
                   (setf current (+ current (parse-integer line))))))
    maximum))

(defun calorie-count-top-3 (list-file)
  (let ((maximum 0)
        (middle 0)
        (least 0)
        (current 0))
    (with-open-file (input list-file)
      (loop for line = (read-line input nil)
            while line do
              (if (= (length line) 0)
                  (cond ((> current maximum)
                         (setf least middle
                               middle maximum
                               maximum current
                               current 0))
                        ((> current middle)
                         (setf least middle
                               middle current
                               current 0))
                        ((> current least)
                         (setf least current
                               current 0))
                        (t (setf current 0)))
                  (setf current (+ current (parse-integer line))))))
    (reduce #'+ (list maximum middle least))))
