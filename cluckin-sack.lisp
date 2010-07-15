(defpackage :cluckin-sack
  (:nicknames :cluck)
  (:use :cl :rucksack))
(in-package :cluckin-sack)

(defvar *cluck-dir* #p"~/cluckin-sack")

(defmacro with-clucktrans (&body body)
  "Wrapper for defining a rucksack transaction using CLUCK-DIR."
  `(with-rucksack (rs *cluck-dir*)
     (with-transaction ()
       ,@body)))

(with-rucksack (rs *cluck-dir*)
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
      (:metaclass persistent-class))))

(with-rucksack (rs *cluck-dir*)
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

       (type         :initarg :type :accessor cluck-type
		     :documentation "Type of entry: in/out")
       
       (notes        :initarg :notes :accessor notes-of
		     :documentation "Free form notes about this entry"))
      (:index t)
      (:metaclass persistent-class))))

;;; Automatically give unique IDs.
(defvar *unique-person-id* 0)
(defmethod initialize-instance :after ((obj person-details) &key)
  (setf (unique-id-of obj) (incf *unique-person-id*)))

(defvar *unique-entry-id* 0)
(defmethod initialize-instance :after ((obj entry-details) &key)
  (setf (unique-id-of obj) (incf *unique-entry-id*)))

;;; Helper methods for printing objects
(defmethod print-object ((obj person-details) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (unique-id ez-id notes) obj
      (format stream "~A: '~A' '~A'"
	      unique-id ez-id notes))))

(defmethod print-object ((obj entry-details) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (unique-id timestamp person type notes) obj
      (format stream "~A: ~A '~A' '~A' '~A'"
	      unique-id timestamp person type notes))))

;;;  Functions to create objects
(defun make-person (ez-id name &optional notes)
  (with-clucktrans
    (make-instance 'person-details 
		   :ez-id (or ez-id "")
		   :name (or name "")
		   :notes notes)))

(defun cluck-in (ez-id &optional notes)
  (with-clucktrans
    (make-instance 'entry-details 
		   :timestamp (get-universal-time)
		   :person (find-person-by-ezid ez-id)
		   :type "in"
		   :notes notes)))

;;; Functions to find/return objects
(defun print-all-persons ()
  (with-clucktrans
    (rucksack-map-class rs 'person-details
			(lambda (object)
			  (format t "~A~%" object)))))

(defun print-all-entries ()
  (with-clucktrans
    (rucksack-map-class rs 'entry-details
			(lambda (object)
			  (format t "~A~%" object)))))

(defun find-person-by-ezid (ez-id)
  (with-clucktrans
    (rucksack-map-slot rs 'person-details 'ez-id
		       (lambda (person)
			 (return-from find-person-by-ezid person))
		       :equal ez-id))
  nil)

(defun find-persons-by-ezid-range (&optional start end)
  (let (ret)
    (with-clucktrans
      (rucksack-map-slot rs 'person-details 'ez-id
			 (lambda (person)
			   (push person ret))
			 :min start :max end :include-min t :include-max t))
    (nreverse ret)))

(defun delete-person-by-ezid (ez-id)
  (with-clucktrans
    (let ((person (find-person-by-ezid ez-id)))
      (when person
	(rucksack::rucksack-delete-object rs person)))))