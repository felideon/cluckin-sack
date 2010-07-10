(defpackage :cluckin-sack
  (:nicknames :cluck)
  (:use :cl :rucksack))
(in-package :cluckin-sack)

(defvar *cluck-dir* #p"/Users/felipe/cluckin-sack")

;; TODO: Remove Supersede after testing
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

;; TODO: Remove Supersede after testing
(with-rucksack (rs *cluck-dir* :if-exists :supersede)
  (with-transaction ()
    (defclass entry-details ()
      ((unique-id    :initarg :unique-id :accessor unique-id-of 
		     :index :number-index
                     :unique t
		     :documentation "A unique number for each entry")
              
       (timestamp    :initarg :timestamp :accessor timestamp-of 
		     :index :number-index
	             :documentation "The timestamp of the entry")

       (person       :initarg :person :accessor person-of
		     :documentation "Person object the entry is assigned to")
       
       (notes        :initarg :notes :accessor notes-of
		     :documentation "Free form notes about this entry"))
      (:index t)
      (:metaclass persistent-class)) 
    ))

;;; Automatically give unique IDs.
(defvar *unique-person-id* 0)
(defmethod initialize-instance :after ((obj person-details) &key)
  (setf (unique-id-of obj) (incf *unique-person-id*)))

(defvar *unique-entry-id* 0)
(defmethod initialize-instance :after ((obj entry-details) &key)
  (setf (unique-id-of obj) (incf *unique-entry-id*)))

;;; Helper method for printing objects
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