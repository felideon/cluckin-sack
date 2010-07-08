(defpackage :cluckin-sack
  (:nicknames :cluck)
  (:use :cl :rucksack))
(in-package :cluckin-sack)

(defvar *cluck-dir* #p"~/cluckin-sack")

(with-rucksack (rs *cluck-dir* :if-exists :supersede)
  (with-transaction ()
    (defclass person-details ()
      ((unique-id    :initarg :unique-id :accessor unique-id-of 
		     :index :number-index
                     :unique t
		     :documentation "A unique number for each person")

       (ez-id        :initarg :ez-id :accessor ez-id-of 
		     :index :case-insensitive-string-index
                     :unique t
		     :documentation "A unique string for quick entry")
       
       (name         :initarg :name :accessor name-of 
		     :index :case-insensitive-string-index
	             :documentation "The full name of the person")
       
       (notes        :initarg :notes :accessor notes-of
		     :documentation "Free form notes about this person"))
      (:index t)
      (:metaclass persistent-class)) 
    ))

;; Automatically give unique ID.
(defvar *unique-id* 0)
(defmethod initialize-instance :after ((obj person-details) &key)
  (setf (unique-id-of obj) (incf *unique-id*)))

;; Helper method for printing a person object
(defmethod print-object ((obj person-details) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (unique-id ez-id notes) obj
	(format stream "~A: '~A' '~A'"
                unique-id ez-id notes))))

(defun make-person (ez-id name &optional notes)
  (with-rucksack (rs *cluck-dir*)
    (with-transaction ()
	(make-instance 'person-details 
		       :ez-id (or ez-id "")
		       :name (or name "")
		       :notes notes))))

(defun print-all-persons ()
  (with-rucksack (rs *cluck-dir*)
    (with-transaction ()
      (rucksack-map-class rs 'person-details
			  (lambda (object)
			    (format t "~A~%" object))))))

(defun find-person-by-ezid (ez-id)
  (with-rucksack (rs *cluck-dir*)
    (with-transaction ()
      (rucksack-map-slot rs 'person-details 'ez-id
			 (lambda (person)
			   (return-from find-person-by-ezid person))
			 :equal ez-id)))
  nil)

(defun find-persons-by-ezid-range (&optional start end)
  (let (ret)
    (with-rucksack (rs *cluck-dir*)
      (with-transaction ()
	(rucksack-map-slot rs 'person-details 'ez-id
			    (lambda (person)
			      (push person ret))
			    :min start :max end :include-min t :include-max t)))
    (nreverse ret)))

(defun delete-object-by-ezid (ez-id)
  (with-rucksack (rs *cluck-dir*)
    (with-transaction ()
      (let ((person (find-person-by-ezid ez-id)))
	(when person
	  (rucksack::rucksack-delete-object rs person))))))