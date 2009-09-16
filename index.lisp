(defpackage :tagfs
 (:nicknames :tagfs)
 (:use :cl :cl-fad :trivial-shell :rucksack))

(in-package :tagfs)

(defmacro aif (test &rest forms)
  `(let ((it ,test))
     (if it ,@forms)))

(defmacro doseq ((var sequence) &body body)
  (let ((index (gensym))
	(seq (gensym)))
    `(let ((,seq ,sequence))
       (dotimes (,index (length ,seq))
	 (let ((,var (elt ,seq,index)))
	   ,@body)))))

(defvar *tagfs-root* #p"/home/illuminati/tagfs-test")

(labels ((split-mime (mime-string)
	   (let ((slash (position #\/ mime-string)))
	     (if (null slash) (error (format nil "Bad mime-type: ~A" 
					     mime-string))
	     (values (subseq mime-string 0 slash)
		     (subseq mime-string slash))))))
 
  (defun derive-filetype (pathname)
    (multiple-value-bind (type subtype)
	(split-mime (shell-command 
		     (format nil 
			     "file --mime-type -b ~A" 
			     (namestring pathname))))
      (cond ((equal type "text") :text)
	    ((equal type "image") :image)
	    ((equal type "audio") :audio)
	    ((equal type "video") :video)
	    ((equal type "application")
	     (cond ((equal subtype "x-executable") :executable)
		   (t :other))
	     )
	    (t :other))))
  
  (setf (get :text 'documentation) "General text-based file")
  (setf (get :image 'documentation) "Image file")
  (setf (get :audio 'documentation) "Audio file")
  (setf (get :video 'documentation) "Video file")
  (setf (get :executable 'documentation) "Executable file")
  (setf (get :other 'documentation) "Unknown or untyped file")
  )

(defun path< (firstpath secondpath)
  (string< (namestring firstpath)
	   (namestring secondpath)))

(define-index-spec :path-index
    '(btree :key< path< 
      :value= p-eql
      :value-type persistent-object
      :key-type pathname))

(with-rucksack (rs *tagfs-root* :if-exists :supersede)
  (with-transaction ()
    (defclass tag ()
      ((file-id      :initarg :file-id :accessor file-id-of
		     :index :number-index
		     :unique t
		     :documentation "Unique identifier")
       (category     :initarg :category :accessor category-of
		     :index :case-insensitive-string-index
		     :initform ""
		     :documentation "Every tag belongs to a category")
       (tag          :initarg :tag :accessor tag-of
		     :index :case-insensitive-string-index
		     :documentation "Actual tag name"))
      (:documentation
       "The tags are what are actually searched and indexed over
        when looking for files.")
      (:index t)
      (:metaclass persistent-class))
      
    (defclass file-details ()
      ((file-id      :initarg :file-id :accessor file-id-of
		     :index :number-index
		     :unique t
		     :documentation "Unique identifier")
       (pathname     :initarg :pathname :accessor pathname-of
		     :index :path-index
		     :unique t
		     :documentation "The full pathname of the file")
       (filename     :initarg :filename :accessor filename-of 
		     :index :string-index
		     :documentation "The name of the file")
       (filetype     :initarg :filetype :accessor filetype-of
		     :index :symbol-index
		     :documentation "The general kind of file this is")
       (notes        :initarg :notes :accessor notes-of
		     :documentation "Free form notes about this file"))
      (:documentation
       "Contains information unique to each files indexed.")
      (:index t)
      (:metaclass persistent-class)) 
    ))

(defun file-by-id (file-id)
  (with-rucksack (rs *tagfs-root*)
    (with-transaction ()
      (rucksack-do-slot (file 'file-details 'file-id :rucksack rs
			      :equal file-id)
	(return-from file-by-id file))))
  nil)

(defun file-by-pathname (pathname)
  (with-rucksack (rs *tagfs-root*)
    (with-transaction ()
      (rucksack-map-slot rs 'file-details pathname'
			 (lambda (file)
			   (return-from file-by-pathname file))
			 :equal pathname)))
  nil)

(defmethod print-object ((tag tag) stream)
  (print-unreadable-object (tag stream :type t)
    (format stream "~A, ~A:~A"
	    (file-id-of tag) 
	    (category-of tag) 
	    (tag-of tag))))

(defmethod print-object ((file-details file-details ) stream)
  (print-unreadable-object (file-details stream :type t)
    (with-slots (file-id pathname filename filetype notes) file-details 
      (format stream "~A: ~A; ~A, ~A, ~A"
	      file-id pathname filename filetype notes))))

(defvar *unique-file-id* 0)
(defmethod initialize-instance :after ((file-details file-details) &key)
  (unless (file-exists-p (pathname-of file-details))
    (error (format nil 
		   "Attempted to add file that doesn't exist.~% file: ~A"
		   (pathname-of file-details))))
  (unless (slot-boundp file-details 'filetype)
    (setf (filetype-of file-details) 
	  (derive-filetype (pathname-of file-details))))
  (setf (file-id-of file-details) (incf *unique-file-id*)))

(defmethod initialize-instance :after ((tag tag) &key)
  (let ((file-id (file-id-of tag)))
    (unless (file-by-id file-id)
      (setf (file-id-of tag) nil)
      (setf (category-of tag) nil)
      (setf (tag-of tag) nil)
      (error (format nil 
		     "Attempted to add tag to file that doesn't exist.~% file-id: ~A~% tag: ~A"
		     file-id tag)))))

(defmethod remove-file ((file-details file-details))
  (with-rucksack (rs *tagfs-root*)
    (with-transaction ()
      (when file-details
	(let ((tags nil))
	  (rucksack-do-slot (tag 'tag 'file-id :rucksack rs 
				  :equal (file-id-of file-details))
	    (push tag tags))
	  (dolist (tag tags)
	    (rucksack-delete-object rs tag)))
	(rucksack-delete-object rs file-details)))))

(defun add-file (pathname &optional (update t) (notes ""))
  (if (and (not (directory-pathname-p pathname))
	   (file-exists-p pathname))
      (with-rucksack (rs *tagfs-root*)
	(with-transaction ()
	  (unless (block nil
		    (rucksack-do-slot 
			(fd 'file-details 'pathname :rucksack rs :equal pathname)
		      (when update
			(setf (filetype-of fd) (derive-filetype pathname))
			(setf (notes-of fd) notes))
		      (return fd)))
	    (make-instance 'file-details 
			   :pathname pathname
			   :filename (pathname-name pathname)
			   :notes notes))))))

(defun init-db (&optional (root *tagfs-root*))
  (walk-directory root #'add-file))

(defun prune-db ()
  (with-rucksack (rs *tagfs-root*)
    (with-transaction ()
      (let ((fds nil))
	(rucksack-do-class (fd 'file-details :rucksack rs)
	  (unless (file-exists-p (pathname-of fd))
	    (push fd fds)))
	(dolist (fd fds)
	  (remove-file fd))))))

(defgeneric tag-file (file tag &optional tag-category)
  (:documentation "Adds a tag to a file"))

(defmethod tag-file ((file number) tag &optional (tag-category ""))
  (make-instance 'tag :file-id file 'tag tag tag-category))

(defmethod tag-file ((file file-details) tag &optional (tag-category ""))
  (tag-file (file-id-of file) tag tag-category))

(defmethod tag-file ((file pathname) tag &optional (tag-category ""))
  (tag-file (file-by-pathname pathname) tag tag-category))

(defun list-files-by-tag (tag &optional tag-category)
  (with-rucksack (rs *tagfs-root*)
    (with-transaction ()
      (let ((files nil))
	(rucksack-do-slot (tg 'tag 'tag :rucksack rs
			       :equal tag)
	  (unless (and tag-category
		       (not (equal (category-of tg) ""))
		       (not (equal (category-of tg) tag-category)))
	    (rucksack-do-slot (file 'file-details 'file-id :rucksack rs
				    :equal (file-id-of tg))
	      (push file files))))
	files))))

;;; User interface operations

(defvar *cli-options*
  #+SBCL sb-ext:*posix-argv*
  #+CCL *command-line-argument-list*
  #+CLISP ext:*args*
  #+LISPWORKS system:*line-arguments-list*
  #+CMU extensions:*command-line-words*
 )

(defvar *bool-params* nil)
(defvar *file-params* nil)

(defvar *parsed-options* nil)
(defvar *files* nil)

(defmacro dispatch-on-param (parameter bool-case file-case)
  `(let ((parameter ,parameter))
     (cond ((find parameter *bool-params* :test #'equal)
	    ,@bool-case)
	   ((find parameter *file-params* :test #'equal)
	    ,@file-case)
	   (t (warn (format "Bad parameter: ~A~%" parameter))))))

(defun push-option (option value)
  (push (cons option value) *parsed-options*))
  
(defun parse-options ()
  "parses an option string '-alf' or '--file'"
  (let ((option (pop *cli-options*)))
    (cond ((scan "^-[^-]+$" option)
	   (doseq (char (subseq option 1))
	     (dispatch-on-param (string char)
				((push-option parameter t))
				((if (= (length option) (1+ (position char option)))
				     (push-option parameter (pop *cli-options*))
				     (push-option parameter 
						  (subseq option (1+ (position char option)))))
				 (return)))))
	  ((scan "^--[^-]+$" option)
	   (dispatch-on-param (subseq option 2)
			      ((push-option parameter (pop *cli-options*)))
			      ((push-option parameter t))))
	  (t (push option *files*))))
  (unless (null *cli-options*) (parse-options)))

'&parameters

(defun parse-cli-options (options option-pattern)
  (let ((*cli-options* options)
	(*bool-params* (subseq option-pattern 0 (position '&parameters option-pattern)))
	(*file-params* (subseq option-pattern 
			       (1+ (position '&parameters option-pattern))))
	(*parsed-options* nil)
	(*files* nil))
    (parse-options)
    (values *parsed-options* *files*)))
