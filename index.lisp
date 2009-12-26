;;; index.lisp - index 2009 (C) Andrew Stine

(defpackage :index
 (:nicknames :index)
 (:use #:cl #:cl-fad #:trivial-shell #:unix-options #:rucksack))

(in-package :index)

(defmacro expand-list (list)
  (if (listp list)
      `(list ,@list)
       list))

(defmacro aif (test &rest forms)
  `(let ((it ,test))
     (if it ,@forms)))

(defmacro doseq ((var sequence) &body body)
  "Loops over a generic sequence executing body for each member"
  (let ((index (gensym))
	(seq (gensym)))
    `(let ((,seq ,sequence))
       (dotimes (,index (length ,seq))
	 (let ((,var (elt ,seq ,index)))
	   ,@body)))))

(defmacro hash (&rest pairs)
  "Returns a hash table of the key value pais provided"
  (unless (evenp (list-length pairs)) (warn "Uneven pairs in hash declairation"))
  `(let ((hash (make-hash-table :test #'equal)))
     ,@(maplist (lambda (key-val)
		  `(setf (gethash ,(first key-val) hash) ,(second key-val)))
		pairs)
     hash))

(defmacro with-gensyms (symbols &body body)
  `(let (,@(mapcar (lambda (symbol) 
		     `(,symbol (gensym)))
		   symbols))
	 ,@body))

(defvar *rucksack-root* #p"/home/illuminati/index-test/"
	"The home directory for the db")
(defvar *index-root* #p"/home/illuminati/media/"
	"The directory specifying the root of the db")

(defun subdirectory (directory sub-directory)
  (unless (directory-pathname-p directory)
    (cerror "Concatinate anyway." "~A is not a directory.~%" directory))
  (unless (directory-pathname-p sub-directory)
    (warn "Converting file path: ~A to directory path.~%" sub-directory))
  (when (equal (first (pathname-directory sub-directory)) :absolute)
    (cerror "Treat as a relative path." "~A is an absolute path.~%" sub-directory))
  (pathname-as-directory (concatenate 'string
				      (namestring directory)
				      (namestring sub-directory))))

(labels ((split-mime (mime-string)
	   "Splits a mime-type string into its type and subtype"
	   (let ((slash (position #\/ mime-string)))
	     (if (null slash) 
		 (warn (format nil "Bad mime-type: ~A" 
			       mime-string))
		 (values (subseq mime-string 0 slash)
			 (subseq mime-string (1+ slash)
				 (1- (length mime-string))))))))
 
  (defun derive-filetype (pathname)
    "Attempts to get the file type of a file specified by pathname.
     Returns a keyword :text, :image, :audio, :video, :executable
     or :other, closest to the actual filetype."
    (multiple-value-bind (type subtype)
	(split-mime (shell-command 
		     (format nil 
			     "file --mime-type -b '~A'" 
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
  "Returns true of firstpath is the alphabetical antecedent of secondpath"
  (string< (namestring firstpath)
	   (namestring secondpath)))

(define-index-spec :path-index
    '(btree :key< path< 
      :value= p-eql
      :value-type persistent-object
      :key-type pathname))

(defun define-classes ()
  "Defines the two main classes in index; Run this in a transaction
   if you want to create a new db"
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
		   :documentation "Free-form notes about this file"))
    (:documentation
     "Contains information unique to each file indexed.")
    (:index t)
    (:metaclass persistent-class)))

(eval-when (:load-toplevel)
  (define-classes))

(defun file-by-id (file-id)
  "pull the first file in the db with given id."
  (with-transaction ()
    (rucksack-do-slot (file 'file-details 'file-id :equal file-id)
      (return-from file-by-id file)))
  nil)

(defun file-by-pathname (pathname)
  "pull the first file in the db with given pathname."
  (with-transaction ()
    (rucksack-map-slot *rucksack* 'file-details 'pathname
		       (lambda (file)
			 (return-from file-by-pathname file))
		       :equal pathname))
  nil)

(defmethod print-object ((tag tag) stream)
  ;tags need to be able to be printed
  (print-unreadable-object (tag stream :type t)
    (format stream "~A, ~A:~A"
	    (file-id-of tag) 
	    (category-of tag) 
	    (tag-of tag))))

(defmethod print-object ((file-details file-details ) stream)
  ;file-details need to be able to be printed
  (print-unreadable-object (file-details stream :type t)
    (with-slots (file-id pathname filename filetype notes) file-details 
      (format stream "~A: ~A; ~A, ~A, ~A"
	      file-id pathname filename filetype notes))))

(defvar *unique-file-id* 0
  "Used to generate unique file-ids")
(defmethod initialize-instance :after ((file-details file-details) &key)
  (print (filename-of file-details))
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

(defgeneric remove-file (file-details)
  (:documentation "Removes a file from the db"))
(defmethod remove-file ((file-details file-details))
  (with-transaction ()
    (when file-details
      (let ((tags nil))
	(rucksack-do-slot (tag 'tag 'file-id :equal (file-id-of file-details))
	  (push tag tags))
	(dolist (tag tags)
	  (rucksack-delete-object *rucksack* tag)))
      (rucksack-delete-object *rucksack* file-details))))

(defun clear-db ()
  (with-transaction ()
    (let ((items nil)
	  (count 0))
      (rucksack-do-class (fd 'file-details)
	(push fd items))
      (dolist (fd items)
	(rucksack-delete-object *rucksack* fd))
      (rucksack-do-class (tag 'tag)
	(push tag items))
      (dolist (tag items)
	(rucksack-delete-object *rucksack* tag))))
  (setf *unique-file-id* 0))

(defun add-file (pathname &optional (update t) (notes ""))
  (format t "Adding file: ~A~%" pathname)
  (if (and (not (directory-pathname-p pathname))
	   (file-exists-p pathname))
      (with-transaction ()
	(unless (block nil
		  (rucksack-do-slot 
		      (fd 'file-details 'pathname :equal pathname)
		    (when update
		      (setf (filetype-of fd) (derive-filetype pathname))
		      (setf (notes-of fd) notes))
		    (return fd)))
	  (make-instance 'file-details 
			 :pathname pathname
			 :filename (pathname-name pathname)
			 :notes notes)))))

(defun init-db (&optional (root *index-root*))
  (format t "Indexing directory: ~A~%" root)
  (with-transaction ()
    (define-classes))
  (walk-directory root #'add-file :directories nil))

(defun prune-db ()
  (with-transaction ()
    (let ((fds nil))
      (rucksack-do-class (fd 'file-details)
	(unless (file-exists-p (pathname-of fd))
	  (push fd fds)))
      (dolist (fd fds)
	(remove-file fd)))))

(defgeneric tag-file (file tag &optional tag-category)
  (:documentation "Adds a tag to a file"))

(defmethod tag-file ((file number) tag &optional (tag-category ""))
  (make-instance 'tag :file-id file :tag tag :category tag-category))

(defmethod tag-file ((file file-details) tag &optional (tag-category ""))
  (tag-file (file-id-of file) tag tag-category))

(defmethod tag-file ((file pathname) tag &optional (tag-category ""))
  (tag-file (file-by-pathname file) tag tag-category))

(defun list-files-by-tag (tag &optional tag-category)
  (with-transaction ()
    (let ((files nil))
      (rucksack-do-slot (tg 'tag 'tag :equal tag)
	(unless (and tag-category
		     (not (equal (category-of tg) ""))
		     (not (equal (category-of tg) tag-category)))
	  (rucksack-do-slot (file 'file-details 'file-id :equal (file-id-of tg))
	    (push file files))))
      files)))

;;; --------------------- User interface operations ----------------------

(defmacro with-index (path &body body)
  `(let* ((*index-root* ,path)
	  (*rucksack-root* (subdirectory *index-root* ".index/")))
     (with-rucksack (*rucksack* *rucksack-root*)
       ,@body)))

(defun toplevel ()
  (with-cli-options () (index tag query &parameters path file &free files)
    (unless path (setf path (pop files)))
    (setf path (pathname path))
    (print index)
    (print tag)
    (print query)
    (print path)
    (print file)
    (setf *index-root* *default-pathname-defaults*)
    (setf *rucksack-root* (subdirectory *index-root* ".index/"))
    (with-rucksack (*rucksack* *index-root*)
      (cond (index (init-db)
		   (prune-db))
	    (tag (tag-file (pathname file) (first files)))
	    (query (print (list-files-by-tag (first files)))))))
  
  #+sbcl(sb-ext:quit)
  )
