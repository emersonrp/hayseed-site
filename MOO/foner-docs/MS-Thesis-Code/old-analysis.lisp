;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Analysis of test run output program

;;; [This file hasn't been touched since Ramstad's implementation, since I'm not
;;; using it for anything.  I haven't even fixed his never-use-a-period runon
;;; sentences in the comments. --- Foner]

;;; this file contains a powerful processor used to analyze the
;;; test runs from the thesis, plus a random routine used to help
;;; get various bits of data into LaTeX format

;;; the program works by getting each structure referenced in the test
;;; run into one of two arrays, *blah-array* for schemas, and
;;; *blah2-array* for everything else

;;; each member of *blah-array* is then categorized (or not) and these
;;; categories are the basis for the output from the program

(in-package :schema)

(defun DOIT ()
  (init)
  (analyze "0s01ana.out" "0s01res.out")
  (init)
  (analyze "0s02ana.out" "0s02res.out")
  (init)
  (analyze "0s03ana.out" "0s03res.out")
  (init)
  (analyze "0s04ana.out" "0s04res.out")
  (init)
  (analyze "0s05ana.out" "0s05res.out")
  (init)
  (analyze "0s06ana.out" "0s06res.out")
  (init)
  (analyze "0s07ana.out" "0s07res.out")
  (init)
  (analyze "0s08ana.out" "0s08res.out")
  (init)
  (analyze "0s09ana.out" "0s09res.out")
  (init)
  (analyze "0s10ana.out" "0s10res.out")
  (init)
  (analyze "0s11ana.out" "0s11res.out")
  (init)
  (analyze "0s12ana.out" "0s12res.out")
  (init)
  (analyze "0s13ana.out" "0s13res.out")
  (init)
  (analyze "0s14ana.out" "0s14res.out")
  (init)
  (analyze "0s15ana.out" "0s15res.out")
  (init)
  (analyze "0s16ana.out" "0s16res.out")
  (init)
  (analyze "0s17ana.out" "0s17res.out")
  (init)
  (analyze "0s18ana.out" "0s18res.out")
  (init)
  (analyze "0s19ana.out" "0s19res.out")
  (init)
  (analyze "0s20ana.out" "0s20res.out"))

(defvar *BLAH-NUMBER* 0)
(defvar *BLAH2-NUMBER* 0)
(defvar *RELIABLE-BLOCK* 0)

(proclaim '(fixnum *blah-number* *blah2-number* *reliable-block*))

(defun INIT ()
  (setf *blah-number* 0)
  (setf *blah2-number* 0)
  (setf *reliable-block* 0))

;;; blah datatype

;;; all the slots in this structure should be self explanatory except
;;; for rel-idx, which is a measure of how reliable the schema is
;;; rel-idx is calculated by dividing the number of times the schema
;;;   was over .9 reliability at the end of a block of 50 clock ticks
;;;   by the number of such blocks that have occurred since the schema
;;;   was first created

(defvar *BLAH-ARRAY* (make-array 5000))
(defmacro GET-BLAH (x) `(aref *blah-array* ,x))

(defvar *BLAH2-ARRAY* (make-array 5000))
(defmacro GET-BLAH2 (x) `(aref *blah2-array* ,x))

(defstruct BLAH
  (creation-time  0 :type fixnum)
  (creation-block  0 :type fixnum)
  (print-name    "" :type string)
  (context       "" :type string)
  (action        "" :type string)
  (result        "" :type string)
  (category      99 :type fixnum)
  (reliable-blocks '() :type list)
  (rel-idx 0.0 :type short-float))

(defun PRINT-BLAH (blah &optional (out-stream t))
  (format out-stream "time: ~5D  schema: ~30A rel-idx: ~5,3F~%"
          (blah-creation-time blah)
          (blah-print-name blah)
          (blah-rel-idx blah)))

(defun PRINT-BLAH2-SHORT (blah &optional (out-stream t))
  (format out-stream "time: ~5D  schema: ~30A~%"
          (blah-creation-time blah)
          (blah-print-name blah)))

(defun PRINT-LIST-OF-FIXNUMS
  (list &optional (out-stream t))
  
  (dolist
   (elem list)
   (format out-stream "~3D " elem))
  (format out-stream "~%"))

;;; blah datatype functions continued

(defun MAKE-AND-STASH-BLAH (&optional category)
  (let ((currentblah (make-blah)))
    (setf (get-blah *blah-number*)
          currentblah
          *blah-number*
          (fix1+ *blah-number*))
    (if category
        (setf (blah-category currentblah) category))
    currentblah))

(defun MAKE-AND-STASH-BLAH2 (&optional category)
  (let ((currentblah (make-blah)))
    (setf (get-blah2 *blah2-number*)
          currentblah
          *blah2-number*
          (fix1+ *blah2-number*))
    (if category
        (setf (blah-category currentblah) category))
    currentblah))

;;; get-category-print-name returns a string for every category number
;;; used by the analysis program

(defun GET-CATEGORY-PRINT-NAME (category)
  (cond ((fix= category 0) "initial schema")
        ((fix= category 1) "grasping schema")
        ((fix= category 2) "visual shift schema")
        ((fix= category 3) "visual shift limit schema")
        ((fix= category 4) "foveal shift schema")
        ((fix= category 5) "coarse to detailed visual shift schema")
        ((fix= category 6) "visual network schema")
        ((fix= category 7) "hand network schema")
        ((fix= category 8) "negative consequence schema")
        ((fix= category 9) "hand to body schema")
        ((fix= category 10)
         "coarse seeing hand motion schema")
        ((fix= category 11)
         "detailed seeing hand motion schema")
        ((fix= category 12)
         "seeing the body via coarse visual items")
        ((fix= category 13)
         "seeing the body via detailed visual items")
        ((fix= category 14)
         "using the body as a visual reference point")
        ((fix= category 15)
         "hand position required for a given hp translation")
        ((fix= category 16)
         "gaze position required for a given vp translation")
        ((fix= category 17)
         "coarse visual item required for a given vf translation")
        ((fix= category 18)
         "coarse visual item required for a given fov translation")
        ((fix= category 19)
         "detailed visual item required for a given vf translation")
        ((fix= category 20)
         "foveal region relationship with coarse visual field")
        ((fix= category 21)
         "breaking up visual region by where objects are seen")
        ((fix= category 22)
         "hand has to be in a given position to touch the body")
        ((fix= category 23)
         "hand cannot push an object out of the way")
        ((fix= category 24)
         "detailed visual item required for a given fov translation")
        ((fix= category 25)
         "hand in front of body and moving against it")
        ((fix= category 26)
         "hand movement relating coarse to detailed visual items")
        (t (error "undefined category code"))))

;;; main routine: analyze

(defun ANALYZE (outfilename &rest infilenames)
  ;; initialize the initial schemas and categorize them
  (let ((handr-blah (make-and-stash-blah 0))
        (handl-blah (make-and-stash-blah 0))
        (handf-blah (make-and-stash-blah 0))
        (handb-blah (make-and-stash-blah 0))
        (eyer-blah (make-and-stash-blah 0))
        (eyel-blah (make-and-stash-blah 0))
        (eyef-blah (make-and-stash-blah 0))
        (eyeb-blah (make-and-stash-blah 0))
        (grasp-blah (make-and-stash-blah 0))
        (ungrasp-blah (make-and-stash-blah 0)))
    (setf (blah-action handr-blah) "handr"
          (blah-action handl-blah) "handl"
          (blah-action handf-blah) "handf"
          (blah-action handb-blah) "handb"
          (blah-action eyer-blah) "eyer"
          (blah-action eyel-blah) "eyel"
          (blah-action eyef-blah) "eyef"
          (blah-action eyeb-blah) "eyeb"
          (blah-action grasp-blah) "grasp"
          (blah-action ungrasp-blah) "ungrasp"
          (blah-print-name handr-blah) "/handr/"
          (blah-print-name handl-blah) "/handl/"
          (blah-print-name handf-blah) "/handf/"
          (blah-print-name handb-blah) "/handb/"
          (blah-print-name eyer-blah) "/eyer/"
          (blah-print-name eyel-blah) "/eyel/"
          (blah-print-name eyef-blah) "/eyef/"
          (blah-print-name eyeb-blah) "/eyeb/"
          (blah-print-name grasp-blah) "/grasp/"
          (blah-print-name ungrasp-blah) "/ungrasp/"
          (blah-creation-block handr-blah) 1
          (blah-creation-block handl-blah) 1
          (blah-creation-block handf-blah) 1
          (blah-creation-block handb-blah) 1
          (blah-creation-block eyer-blah) 1
          (blah-creation-block eyel-blah) 1
          (blah-creation-block eyef-blah) 1
          (blah-creation-block eyeb-blah) 1
          (blah-creation-block grasp-blah) 1
          (blah-creation-block ungrasp-blah) 1))
  ;; open the output file
  (with-open-file
   (out-file outfilename
             :direction :output
             :if-exists :supersede)

;;; analyze routine continued
     
   ;; process the input files in succession
   (dolist
    (infile infilenames)
    (with-open-file
     (in-file infile
              :direction :input)
     ;; process each input file, creating blah structures for each
     ;; schema encountered, and blah2 structures for everything else
     ;; when reliability info is encountered, append it to the
     ;; appropriate blah structure for later rel-idx calculation
     (do ((time -1)
          (line nil)
          (status nil))
         ((progn
            (setf time (read in-file nil nil))
            (multiple-value-setq (line status) (read-line in-file nil nil))
            (if line status t))
          "Thanks for using analyze, hope it was helpful!")
         (cond ((eq time 'reliable)
                (setf *reliable-block* (fix1+ *reliable-block*))
                (format t "finished through time ~5D~%"
                        (fix* 50 *reliable-block*))
                (do ((schema-number-string nil (read-line in-file nil nil)))
                    ((string= "" schema-number-string) "finished")
                    (if schema-number-string
                        (let ((schema-number
                               (read-from-string schema-number-string)))
                          (setf (blah-reliable-blocks
                                 (get-blah schema-number))
                                (append
                                 (blah-reliable-blocks
                                  (get-blah schema-number))
                                 (list *reliable-block*)))))))
               ((string= "composite-" (subseq line 0 10))
                (let ((new-blah (make-and-stash-blah2)))
                  (setf (blah-creation-time new-blah) time
                        (blah-creation-block new-blah) *reliable-block*
                        (blah-category new-blah) 0
                        (blah-print-name new-blah)
                        (string-append "/<" (line-schema line) ">/"))))
               ((string= "syn item " (subseq line 0 9))
                (let ((new-blah (make-and-stash-blah2)))
                  (setf (blah-creation-time new-blah) time
                        (blah-creation-block new-blah) *reliable-block*
                        (blah-category new-blah) 1
                        (blah-print-name new-blah) (line-schema line))))
               (t
                (let ((new-blah (make-and-stash-blah)))
                  (setf (blah-creation-time new-blah) time
                        (blah-creation-block new-blah) *reliable-block*
                        (blah-print-name new-blah) (line-schema line))))))))

;;; analyze routine continued
     
   ;; blah-update moves the components of print-name into the context,
   ;; action and result slots, and also calculates the rel-idx
   (dotimes
    (x *blah-number*)
    (let ((blah (get-blah x)))
      (blah-update blah)))
   
   (dotimes
    (x *blah2-number*)
    (let ((blah (get-blah2 x)))
      (blah-update blah)))

   ;; categorize all the elements in the *blah-array*
   (dotimes
    (x *blah-number*)
    (let* ((blah (get-blah x))
           (context (blah-context blah))
           (action (blah-action blah))
           (result (blah-result blah))
           (print-name (blah-print-name blah)))
      (declare (string context action result))
      (let ((context-len (length context))
            (action-len (length action))
            (result-len (length result))
            (print-len (length print-name)))
        (declare (fixnum context-len action-len result-len))

;;; analyze routine continued
     
        ;; if not already categorized
        (if (fix= 99 (blah-category blah))
            ;; place in one of the following categories
            (cond
             ;; any schema we don't want to see, set it's category to 98
             ((or (string= (subseq print-name 0 1) "[")
                  (string= (subseq print-name 0 2) "-[")
                  (string=
                   (subseq print-name (fix1- print-len) print-len)
                   "]"))
              (setf (blah-category blah) 98))
             ;; vf??/eye?/vf?? same items
             ((and (fix= 4 context-len)
                   (fix= 4 action-len)
                   (fix= 4 result-len)
                   (string= "vf" (subseq context 0 2))
                   (string= "eye" (subseq action 0 3))
                   (string= "vf" (subseq result 0 2))
                   (string= context result))
              (setf (blah-category blah) 98))
             ;; -vf??/eye?/-vf?? same items
             ((and (fix= 5 context-len)
                   (fix= 4 action-len)
                   (fix= 5 result-len)
                   (string= "-vf" (subseq context 0 3))
                   (string= "eye" (subseq action 0 3))
                   (string= "-vf" (subseq result 0 3))
                   (string= context result))
              (setf (blah-category blah) 98))
             ;; fov???/eye?/fov??? and same fov letter
             ((and (fix= 6 context-len)
                   (fix= 4 action-len)
                   (fix= 6 result-len)
                   (string= "fov" (subseq context 0 3))
                   (string= "eye" (subseq action 0 3))
                   (string= "fov" (subseq result 0 3))
                   (string= (subseq context 0 4) (subseq result 0 4)))
              (setf (blah-category blah) 98))
             ;; -hp??/hand?/hp??
             ((and (fix= 5 context-len)
                   (fix= 5 action-len)
                   (fix= 4 result-len)
                   (string= "-hp" (subseq context 0 3))
                   (string= "hand" (subseq action 0 4))
                   (string= "hp" (subseq result 0 2)))
              (setf (blah-category blah) 98))

;;; analyze routine categorization continued
     
             ;; grasping schemas
             ;; */grasp/*, */ungrasp/*
             ((or (string= "grasp" action)
                  (string= "ungrasp" action))
              (setf (blah-category blah) 1))
             
             ;; visual shift schemas
             ;; vf??/eye?/vf??
             ;; two items adjacent based on eye movement
             ((and (fix= 4 context-len)
                   (fix= 4 action-len)
                   (fix= 4 result-len)
                   (string= "vf" (subseq context 0 2))
                   (string= "eye" (subseq action 0 3))
                   (string= "vf" (subseq result 0 2))
                   (relationship-vf-eye-vf (subseq context 2 4)
                                           (subseq action 3 4)
                                           (subseq result 2 4)))
              (setf (blah-category blah) 2))

;;; analyze routine categorization continued
     
             ;; visual shift limit schemas
             ;; vf??&-vp??/eye?/vf??
             ;; two items adjacent based on eye movement
             ;; and -vp is valid maximum for the eye movement
             ;; first digit 0 for l, 2 for r
             ;; second digit 0 for b, 2 for f
             ((and (fix= 10 context-len)
                   (fix= 4 action-len)
                   (fix= 4 result-len)
                   (string= "eye" (subseq action 0 3))
                   (string= "vf" (subseq result 0 2))
                   (or (and (string= "vf" (subseq context 0 2))
                            (string= "&-vp" (subseq context 4 8))
                            (relationship-vf-eye-vf
                             (subseq context 2 4)
                             (subseq action 3 4)
                             (subseq result 2 4))
                            (or (and (string= (subseq action 3 4) "b")
                                     (string= (subseq context 9 10) "0"))
                                (and (string= (subseq action 3 4) "f")
                                     (string= (subseq context 9 10) "2"))
                                (and (string= (subseq action 3 4) "l")
                                     (string= (subseq context 8 9) "0"))
                                (and (string= (subseq action 3 4) "r")
                                     (string= (subseq context 8 9) "2"))))
                       (and (string= "-vp" (subseq context 0 3))
                            (string= "&vf" (subseq context 5 8))
                            (relationship-vf-eye-vf
                             (subseq context 8 10)
                             (subseq action 3 4)
                             (subseq result 2 4))
                            (or (and (string= (subseq action 3 4) "b")
                                     (string= (subseq context 4 5) "0"))
                                (and (string= (subseq action 3 4) "f")
                                     (string= (subseq context 4 5) "2"))
                                (and (string= (subseq action 3 4) "l")
                                     (string= (subseq context 3 4) "0"))
                                (and (string= (subseq action 3 4) "r")
                                     (string= (subseq context 3 4) "2"))))))
              (setf (blah-category blah) 3))

;;; analyze routine categorization continued
     
             ;; foveal shift schemas
             ;; fov???*/eye?/fov???
             ;; two foveal letters adjacent
             ((and (fix< 5 context-len)
                   (fix= 4 action-len)
                   (fix= 6 result-len)
                   (string= "fov" (subseq context 0 3))
                   (string= "eye" (subseq action 0 3))
                   (string= "fov" (subseq result 0 3))
                   (relationship-fov-eye-fov
                    (subseq context 3 4)
                    (subseq action 3 4)
                    (subseq result 3 4)))
              (setf (blah-category blah) 4))

;;; analyze routine categorization continued
     
             ;; detail shift schemas
             ;; vf??/eye?/fov???
             ;; correct relationship between coarse and detailed
             ;; vf??&fov???/eye?/fov???
             ;; two foveal letters adjacent
             ((and (fix= 4 context-len)
                   (fix= 4 action-len)
                   (fix= 6 result-len)
                   (string= "vf" (subseq context 0 2))
                   (string= "eye" (subseq action 0 3))
                   (string= "fov" (subseq result 0 3))
                   (relationship-vf-eye-fov (subseq context 2 4)
                                            (subseq action 3 4)
                                            (subseq result 3 4)))
              (setf (blah-category blah) 5))
             ((and (fix= 11 context-len)
                   (fix= 4 action-len)
                   (fix= 6 result-len)
                   (string= "eye" (subseq action 0 3))
                   (string= "fov" (subseq result 0 3))
                   (or (and (string= "vf" (subseq context 0 2))
                            (string= "&fov" (subseq context 4 8))
                            (relationship-vf-eye-fov
                             (subseq context 2 4)
                             (subseq action 3 4)
                             (subseq result 3 4))
                            (relationship-fov-eye-fov
                             (subseq context 8 9)
                             (subseq action 3 4)
                             (subseq result 3 4)))
                       (and (string= "fov" (subseq context 0 3))
                            (string= "&vf" (subseq context 5 8))
                            (relationship-vf-eye-fov
                             (subseq context 8 10)
                             (subseq action 3 4)
                             (subseq result 3 4))
                            (relationship-fov-eye-fov
                             (subseq context 3 4)
                             (subseq action 3 4)
                             (subseq result 3 4)))))
              (setf (blah-category blah) 5))

;;; analyze routine categorization continued
     
             ;; visual network schemas
             ;; vp??/eye?/vp??
             ;; two items adjacent or identical (gaze limit)
             ;; vf??/eye?/vp?? or
             ;; vf??&vp??/eye?/vp??
             ;; two items adjacent or identical (gaze limit)
             ;; vf is due to body relationship
             ((and (fix= 4 context-len)
                   (fix= 4 action-len)
                   (fix= 4 result-len)
                   (string= "vp" (subseq context 0 2))
                   (string= "eye" (subseq action 0 3))
                   (string= "vp" (subseq result 0 2))
                   (relationship-vp-eye-vp (subseq context 2 4)
                                           (subseq action 3 4)
                                           (subseq result 2 4)))
              (setf (blah-category blah) 6))
             ((and (fix= 4 action-len)
                   (fix= 4 result-len)
                   (string= "eye" (subseq action 0 3))
                   (string= "vp" (subseq result 0 2))
                   (or (and (fix= 4 context-len)
                            (string= "vf" (subseq context 0 2))
                            (relationship-vf-eye-vp
                             (subseq context 2 4)
                             (subseq action 3 4)
                             (subseq result 2 4)))
                       (and (fix= 9 context-len)
                            (string= "vf" (subseq context 0 2))
                            (string= "&vp" (subseq context 4 7))
                            (relationship-vf-eye-vp
                             (subseq context 2 4)
                             (subseq action 3 4)
                             (subseq result 2 4))
                            (relationship-vp-eye-vp
                             (subseq context 7 9)
                             (subseq action 3 4)
                             (subseq result 2 4)))
                       (and (fix= 9 context-len)
                            (string= "vp" (subseq context 0 2))
                            (string= "&vf" (subseq context 4 7))
                            (relationship-vf-eye-vp
                             (subseq context 7 9)
                             (subseq action 3 4)
                             (subseq result 2 4))
                            (relationship-vp-eye-vp
                             (subseq context 2 4)
                             (subseq action 3 4)
                             (subseq result 2 4)))))
              (setf (blah-category blah) 6))

;;; analyze routine categorization continued
     
             ;; hand network schemas
             ;; hp??/hand?/hp??
             ;; two items adjacent or identical
             ;; (taste? or tactb or bodyf)/hand?/hp??
             ((and (fix= 4 context-len)
                   (fix= 5 action-len)
                   (fix= 4 result-len)
                   (string= "hp" (subseq context 0 2))
                   (string= "hand" (subseq action 0 4))
                   (string= "hp" (subseq result 0 2))
                   (relationship-hp-hand-hp
                    (subseq context 2 4)
                    (subseq action 4 5)
                    (subseq result 2 4)))
              (setf (blah-category blah) 7))
             ((and (fix= 5 action-len)
                   (fix= 4 result-len)
                   (string= "hand" (subseq action 0 4))
                   (string= "hp" (subseq result 0 2))
                   (or (and (fix= 6 context-len)
                            (string= "taste" (subseq context 0 5)))
                       (string= "tactb" context)
                       (string= "bodyf" context))
                   (relationship-body-hand-hp
                    (subseq action 4 5)
                    (subseq result 2 4)))
              (setf (blah-category blah) 7))

             ;; negative consequence schemas
             ;; x/eye?/-x or x/hand?/-x
             ((and (string= result (string-append "-" context))
                   (or (and (fix= 4 action-len)
                            (string= "eye" (subseq action 0 3)))
                       (and (fix= 5 action-len)
                            (string= "hand" (subseq action 0 4)))))
              (setf (blah-category blah) 8))
             
             ;; hand to body schemas
             ;; hp??/hand?/(taste? or tactb or bodyf)
             ((and (fix= 4 context-len)
                   (fix= 5 action-len)
                   (string= "hp" (subseq context 0 2))
                   (string= "hand" (subseq action 0 4))
                   (relationship-hp-hand-body
                    (subseq context 2 4)
                    (subseq action 4 5))
                   (or (and (fix= 6 result-len)
                            (string= "taste" (subseq result 0 5)))
                       (string= "tactb" result)
                       (string= "bodyf" result)))
              (setf (blah-category blah) 9))

;;; analyze routine categorization continued
     
             ;; coarse seeing hand motion schemas
             ;; vf??/hand?/vf??
             ;; the two items relate to one another correctly
             ((and (fix= 4 context-len)
                   (fix= 5 action-len)
                   (fix= 4 result-len)
                   (string= "vf" (subseq context 0 2))
                   (string= "hand" (subseq action 0 4))
                   (string= "vf" (subseq result 0 2))
                   (relationship-vf-hand-vf
                    (subseq context 2 4)
                    (subseq action 4 5)
                    (subseq result 2 4)))
              (setf (blah-category blah) 10))
             
             ;; detailed seeing hand motion schemas
             ;; fov???*/hand?/fov???*
             ;; foveal regions related correctly
             ((and (fix< 5 context-len)
                   (fix= 5 action-len)
                   (fix< 5 result-len)
                   (string= "fov" (subseq context 0 3))
                   (string= "hand" (subseq action 0 4))
                   (string= "fov" (subseq result 0 3))
                   (relationship-fov-hand-fov
                    (subseq context 3 4)
                    (subseq action 4 5)
                    (subseq result 3 4)))
              (setf (blah-category blah) 11))

             ;; seeing the body coarse
             ;; vp??/eye?/vf??
             ;; each correct relative to the body position
             ((and (fix= 4 context-len)
                   (fix= 4 action-len)
                   (fix= 4 result-len)
                   (string= "vp" (subseq context 0 2))
                   (string= "eye" (subseq action 0 3))
                   (string= "vf" (subseq result 0 2))
                   (relationship-vp-eye-vf
                    (subseq context 2 4)
                    (subseq action 3 4)
                    (subseq result 2 4)))
              (setf (blah-category blah) 12))

;;; analyze routine categorization continued
     
             ;; seeing the body detailed
             ;; vp??/eye?/fov???
             ;; each correct relative to the body position
             ((and (fix= 4 context-len)
                   (fix= 4 action-len)
                   (fix= 6 result-len)
                   (string= "vp" (subseq context 0 2))
                   (string= "eye" (subseq action 0 3))
                   (string= "fov" (subseq result 0 3))
                   (relationship-vp-eye-fov
                    (subseq context 2 4)
                    (subseq action 3 4)
                    (subseq result 3 4)))
              (setf (blah-category blah) 13))

             ;; body as visual reference point
             ;; -fov???/eye?/-vp?? or
             ;; fov???/eye?/vp??
             ((and (fix= 7 context-len)
                   (fix= 4 action-len)
                   (fix= 5 result-len)
                   (string= "-fov" (subseq context 0 4))
                   (string= "eye" (subseq action 0 3))
                   (string= "-vp" (subseq result 0 3))
                   (relationship-fov-eye-vp
                    (subseq context 4 5)
                    (subseq action 3 4)
                    (subseq result 3 5)))
              (setf (blah-category blah) 14))
             ((and (fix= 6 context-len)
                   (fix= 4 action-len)
                   (fix= 4 result-len)
                   (string= "fov" (subseq context 0 3))
                   (string= "eye" (subseq action 0 3))
                   (string= "vp" (subseq result 0 2))
                   (relationship-fov-eye-vp
                    (subseq context 3 4)
                    (subseq action 3 4)
                    (subseq result 2 4)))
              (setf (blah-category blah) 14))

;;; analyze routine categorization continued
     
             ;; hand position required for hp transition
             ;; -hp??/hand?/-hp??
             ;; items adjacent and not identical
             ;; (-taste? or -tactb or -bodyf)/hand?/-hp??
             ((and (fix= 5 context-len)
                   (fix= 5 action-len)
                   (fix= 5 result-len)
                   (string= "-hp" (subseq context 0 3))
                   (string= "hand" (subseq action 0 4))
                   (string= "-hp" (subseq result 0 3))
                   (relationship-hp-hand-hp
                    (subseq context 3 5)
                    (subseq action 4 5)
                    (subseq result 3 5)))
              (setf (blah-category blah) 15))
             ((and (fix= 5 action-len)
                   (fix= 5 result-len)
                   (string= "hand" (subseq action 0 4))
                   (string= "-hp" (subseq result 0 3))
                   (or (and (fix= 7 context-len)
                            (string= "-taste" (subseq context 0 6)))
                       (string= "-tactb" context)
                       (string= "-bodyf" context))
                   (relationship-body-hand-hp
                    (subseq action 4 5)
                    (subseq result 3 5)))
              (setf (blah-category blah) 15))

             ;; gaze position required for vp transition
             ;; -vp??/eye?/-vp??
             ((and (fix= 5 context-len)
                   (fix= 4 action-len)
                   (fix= 5 result-len)
                   (string= "-vp" (subseq context 0 3))
                   (string= "eye" (subseq action 0 3))
                   (string= "-vp" (subseq result 0 3))
                   (relationship-vp-eye-vp
                    (subseq context 3 5)
                    (subseq action 3 4)
                    (subseq result 3 5)))
              (setf (blah-category blah) 16))

;;; analyze routine categorization continued
     
             ;; coarse visual item required for vf transition
             ;; -vf??/eye?/-vf??
             ((and (fix= 5 context-len)
                   (fix= 4 action-len)
                   (fix= 5 result-len)
                   (string= "-vf" (subseq context 0 3))
                   (string= "eye" (subseq action 0 3))
                   (string= "-vf" (subseq result 0 3))
                   (relationship-vf-eye-vf
                    (subseq context 3 5)
                    (subseq action 3 4)
                    (subseq result 3 5)))
              (setf (blah-category blah) 17))

             ;; coarse visual item required for fov transition
             ;; -vf??/eye?/-fov???
             ((and (fix= 5 context-len)
                   (fix= 4 action-len)
                   (fix= 7 result-len)
                   (string= "-vf" (subseq context 0 3))
                   (string= "eye" (subseq action 0 3))
                   (string= "-fov" (subseq result 0 4))
                   (relationship-vf-eye-fov
                    (subseq context 3 5)
                    (subseq action 3 4)
                    (subseq result 4 5)))
              (setf (blah-category blah) 18))

             ;; detailed visual item required for vf transition
             ;; -fov???/eye?/-vf???
             ((and (fix= 7 context-len)
                   (fix= 4 action-len)
                   (fix= 5 result-len)
                   (string= "-fov" (subseq context 0 4))
                   (string= "eye" (subseq action 0 3))
                   (string= "-vf" (subseq result 0 3))
                   (relationship-fov-eye-vf
                    (subseq context 4 5)
                    (subseq action 3 4)
                    (subseq result 3 5)))
              (setf (blah-category blah) 19))

;;; analyze routine categorization continued
     
             ;; relationship between foveal regions and coarse visual field
             ;; fov???/eye?/vf??
             ((and (fix= 6 context-len)
                   (fix= 4 action-len)
                   (fix= 4 result-len)
                   (string= "fov" (subseq context 0 3))
                   (string= "eye" (subseq action 0 3))
                   (string= "vf" (subseq result 0 2))
                   (relationship-fov-eye-vf
                    (subseq context 3 4)
                    (subseq action 3 4)
                    (subseq result 2 4)))
              (setf (blah-category blah) 20))

             ;; breaking up the visual region by where objects are seen
             ;; vp/eye?/vf or fov 
             ;; -vp??/eye?/(-vf?? or -fov???)
             ((and (fix= 4 context-len)
                   (fix= 4 action-len)
                   (fix= 4 result-len)
                   (string= "vp" (subseq context 0 2))
                   (string= "eye" (subseq action 0 3))
                   (string= "vf" (subseq result 0 2)))
              (setf (blah-category blah) 21))
             ((and (fix= 4 context-len)
                   (fix= 4 action-len)
                   (fix= 6 result-len)
                   (string= "vp" (subseq context 0 2))
                   (string= "eye" (subseq action 0 3))
                   (string= "fov" (subseq result 0 3)))
              (setf (blah-category blah) 21))
             ((and (fix= 5 context-len)
                   (fix= 4 action-len)
                   (string= "-vp" (subseq context 0 3))
                   (string= "eye" (subseq action 0 3))
                   (or (and (fix= 5 result-len)
                            (string= "-vf" (subseq result 0 3)))
                       (and (fix= 7 result-len)
                            (string= "-fov" (subseq result 0 4)))))
              (setf (blah-category blah) 21))

;;; analyze routine categorization continued
     
             ;; hand has to be in a given position to touch the body
             ;; -hp??/hand?/(-taste? or -tactb or -bodyf)
             ((and (fix= 5 context-len)
                   (fix= 5 action-len)
                   (string= "-hp" (subseq context 0 3))
                   (string= "hand" (subseq action 0 4))
                   (relationship-hp-hand-body
                    (subseq context 3 5)
                    (subseq action 4 5))
                   (or (and (fix= 7 result-len)
                            (string= "-taste" (subseq result 0 6)))
                       (and (fix= 6 result-len)
                            (or (string= "-tactb" (subseq result 0 6))
                                (string= "-bodyf" (subseq result 0 6))))))
              (setf (blah-category blah) 22))

             ;; hand cannot push an object out of the way
             ;; or, if holding it, moves it along with it
             ;; tact?/hand?/tact? and ? is the same or
             ;; tactl/handl/text? or text?/handl/tactl
             ((and (fix= 5 context-len)
                   (fix= 5 action-len)
                   (fix= 5 result-len)
                   (or (and (string= context result)
                            (string= "tact" (subseq context 0 4))
                            (string= "hand" (subseq action 0 4))
                            (string= (subseq context 4 5)
                                     (subseq action 4 5)))
                       (and (string= context "tactl")
                            (string= action "handl")
                            (string= "text" (subseq result 0 4)))
                       (and (string= "text" (subseq context 0 4))
                            (string= action "handl")
                            (string= result "tactl"))))
              (setf (blah-category blah) 23))

;;; analyze routine categorization continued
     
             ;; required fov item for fov result schemas
             ;; -fov???/eye?/-fov???
             ;; two foveal letters adjacent
             ((and (fix= 7 context-len)
                   (fix= 4 action-len)
                   (fix= 7 result-len)
                   (string= "-fov" (subseq context 0 4))
                   (string= "eye" (subseq action 0 3))
                   (string= "-fov" (subseq result 0 4))
                   (relationship-fov-eye-fov
                    (subseq context 4 5)
                    (subseq action 3 4)
                    (subseq result 4 5)))
              (setf (blah-category blah) 24))

             ;; hand in front of body and moving against it
             ;; (tactb or bodyf or taste?)/handb/(tactb or bodyf or taste?)
             ((and (fix< 4 context-len)
                   (fix= 5 action-len)
                   (fix< 4 result-len)
                   (string= "handb" action)
                   (or (string= "tactb" context)
                       (string= "bodyf" context)
                       (string= "taste" (subseq context 0 5)))
                   (or (string= "tactb" result)
                       (string= "bodyf" result)
                       (string= "taste" (subseq result 0 5))))
              (setf (blah-category blah) 25))

;;; analyze routine categorization continued
     
             ;; coarse hand moves shows detail
             ;; vf??/hand?/fov???
             ;; detailed hand moves shows coarse
             ;; fov???/hand?/vf??
             ;; items related correctly
             ((and (fix= 5 action-len)
                   (string= "hand" (subseq action 0 4))
                   (or (and (fix= 4 context-len)
                            (string= "vf" (subseq context 0 2))
                            (fix= 6 result-len)
                            (string= "fov" (subseq result 0 3))
                            (relationship-vf-hand-fov
                             (subseq context 2 4)
                             (subseq action 4 5)
                             (subseq result 3 4)))
                       (and (fix= 6 context-len)
                            (string= "fov" (subseq context 0 3))
                            (fix= 4 result-len)
                            (string= "vf" (subseq result 0 2))
                            (relationship-fov-hand-vf
                             (subseq context 3 4)
                             (subseq action 4 5)
                             (subseq result 2 4)))))
              (setf (blah-category blah) 26))

             (t nil))))))

;;; analyze routine continued

   ;; finished categorizing, so do output
   ;; start by outputting a statistical summary for the test run
   (dotimes
    (category 27)
    (let ((elems 0)
          (rel-elems 0))
      (declare (fixnum elems rel-elems))
      (dotimes
       (x *blah-number*)
       (let ((blah (get-blah x)))
         (if (fix= (blah-category blah) category)
             (if (= (blah-rel-idx blah) 0.0)
                 (setf elems (fix1+ elems))
               (setf rel-elems (fix1+ rel-elems)
                     elems (fix1+ elems))))))
      (format out-file "category ~2D: ~60A~%"
              category (get-category-print-name category))
      (format out-file "             schemas: ~3D reliable schemas: ~3D~%"
              elems rel-elems)))
   (format out-file "~%~%")
   
   ;; then output the schemas belonging to each category
   (dotimes
    (category 27)
    (format out-file "category ~2D: ~60A~%"
            category (get-category-print-name category))
    (dotimes
     (x *blah-number*)
     (let ((blah (get-blah x)))
       (if (fix= category (blah-category blah))
           (print-blah blah out-file))))
    (format out-file "~%"))

   ;; finally, output other interesting schemas
   (format out-file
           "*** non-categorized non-empty context rel-idx above .7 ***~%")
   (dotimes
    (x *blah-number*)
    (let ((blah (get-blah x)))
      (if (and (fix= 99 (blah-category blah))
               (> (blah-rel-idx blah) .7)
               (fix/= 0 (length (blah-context blah))))
          (print-blah blah out-file))))
   (format out-file "~%")
   ))

;;; misc functions used by analyze

(defun LINE-SCHEMA (line)
  (let ((toggle nil)
        (end -1))
    (do* ((x        (fix1- (length line))   (fix1- x))
          (currchar (elt line x)            (elt line x)))
         ((and toggle (char= #\Space currchar)) (subseq line (fix1+ x) end))
         (cond ((and (not toggle)
                     (not (char= #\Space currchar)))
                (setf end (fix1+ x)
                      toggle t))
               (t nil)))))

(defun BLAH-UPDATE (blah)
  (let ((toggle nil)
        (start 0)
        (line (blah-print-name blah)))
    (declare (string line))
    (setf line (delete #\> line :test #'char=))
    (setf line (delete #\< line :test #'char=))
    (setf line (delete #\] line :test #'char=))
    (setf line (delete #\[ line :test #'char=))
    (dotimes
     (x (length line))
     (let ((currentchar (elt line x)))
       (cond ((char= #\/ currentchar)
              (let ((str (subseq line start x)))
                (setf start (fix1+ x))
                (if toggle
                    (setf (blah-action blah)
                          str
                          (blah-result blah)
                          (subseq line (fix1+ x) (length line))
                          toggle
                          nil)
                  (setf (blah-context blah) str
                        toggle t)))))))
    (setf (blah-rel-idx blah)
          (float (/ (length (blah-reliable-blocks blah))
                    (fix1+ (fix- *reliable-block*
                                 (blah-creation-block blah))))))))

;;; functions used by analyze to express microworld relationships continued
;;; these should be easy to understand if the use of them is examined
;;;   and the examiner has a good grasp of microworld mechanics

;; relate coarse visual movement with gaze actions
(defun RELATIONSHIP-VF-EYE-VF (vf1 eye vf2)
  (let ((x1 (read-from-string (subseq vf1 0 1)))
        (y1 (read-from-string (subseq vf1 1 2)))
        (x2 (read-from-string (subseq vf2 0 1)))
        (y2 (read-from-string (subseq vf2 1 2))))
    (declare (fixnum x1 y1 x2 y2))
    (or (and (string= eye "l")
             (fix= (fix1+ x1) x2)
             (fix= y1 y2))
        (and (string= eye "r")
             (fix= x1 (fix1+ x2))
             (fix= y1 y2))
        (and (string= eye "b")
             (fix= x1 x2)
             (fix= (fix1+ y1) y2))
        (and (string= eye "f")
             (fix= x1 x2)
             (fix= y1 (fix1+ y2))))))

;; relate coarse visual movement with hand actions
(defun RELATIONSHIP-VF-HAND-VF (vf1 hand vf2)
  (let ((x1 (read-from-string (subseq vf1 0 1)))
        (y1 (read-from-string (subseq vf1 1 2)))
        (x2 (read-from-string (subseq vf2 0 1)))
        (y2 (read-from-string (subseq vf2 1 2))))
    (declare (fixnum x1 y1 x2 y2))
    (or (and (string= hand "l")
             (fix= x1 (fix1+ x2))
             (fix= y1 y2))
        (and (string= hand "r")
             (fix= (fix1+ x1) x2)
             (fix= y1 y2))
        (and (string= hand "b")
             (fix= x1 x2)
             (fix= y1 (fix1+ y2)))
        (and (string= hand "f")
             (fix= x1 x2)
             (fix= (fix1+ y1) y2)))))

;;; functions used by analyze to express microworld relationships continued

;; relate movement among foveal regions with gaze shift
;; two foveal regions are adjacent if one is "x" and the other is
;; any of "f" "b" "r" or "l"
(defun RELATIONSHIP-FOV-EYE-FOV (fov1 eye fov2)
  (and (not (string= fov1 fov2))
       (or (and (string= fov1 "x")
                (or (and (string= eye "l")
                         (string= fov2 "r"))
                    (and (string= eye "r")
                         (string= fov2 "l"))
                    (and (string= eye "b")
                         (string= fov2 "f"))
                    (and (string= eye "f")
                         (string= fov2 "b"))))
           (and (string= fov2 "x")
                (or (and (string= eye "l")
                         (string= fov1 "l"))
                    (and (string= eye "r")
                         (string= fov1 "r"))
                    (and (string= eye "b")
                         (string= fov1 "b"))
                    (and (string= eye "f")
                         (string= fov1 "f")))))))

;; relate movement among foveal regions with hand shift
(defun RELATIONSHIP-FOV-HAND-FOV (fov1 hand fov2)
  (and (not (string= fov1 fov2))
       (or (and (string= fov1 "x")
                (or (and (string= hand "l")
                         (string= fov2 "l"))
                    (and (string= hand "r")
                         (string= fov2 "r"))
                    (and (string= hand "b")
                         (string= fov2 "b"))
                    (and (string= hand "f")
                         (string= fov2 "f"))))
           (and (string= fov2 "x")
                (or (and (string= hand "l")
                         (string= fov1 "r"))
                    (and (string= hand "r")
                         (string= fov1 "l"))
                    (and (string= hand "b")
                         (string= fov1 "f"))
                    (and (string= hand "f")
                         (string= fov1 "b")))))))

;;; functions used by analyze to express microworld relationships continued

;; relate coarse visual field to detailed visual field
(defun RELATIONSHIP-VF-EYE-FOV (vf eye fov)
  (let ((xvf (read-from-string (subseq vf 0 1)))
        (yvf (read-from-string (subseq vf 1 2))))
    (declare (fixnum xvf yvf))
    (cond ((string= eye "l") (setf xvf (fix1+ xvf)))
          ((string= eye "r") (setf xvf (fix1- xvf)))
          ((string= eye "b") (setf yvf (fix1+ yvf)))
          ((string= eye "f") (setf yvf (fix1- yvf)))
          (t nil))
    (or (and (string= fov "x")
             (fix= xvf 2)
             (fix= yvf 2))
        (and (string= fov "l")
             (fix= xvf 1)
             (fix= yvf 2))
        (and (string= fov "r")
             (fix= xvf 3)
             (fix= yvf 2))
        (and (string= fov "b")
             (fix= xvf 2)
             (fix= yvf 1))
        (and (string= fov "f")
             (fix= xvf 2)
             (fix= yvf 3)))))
        
;; relate detailed visual field to coarse visual field
(defun RELATIONSHIP-FOV-EYE-VF (fov eye vf)
  (let ((xvf (read-from-string (subseq vf 0 1)))
        (yvf (read-from-string (subseq vf 1 2))))
    (declare (fixnum xvf yvf))
    (cond ((string= eye "l") (setf xvf (fix1- xvf)))
          ((string= eye "r") (setf xvf (fix1+ xvf)))
          ((string= eye "b") (setf yvf (fix1- yvf)))
          ((string= eye "f") (setf yvf (fix1+ yvf)))
          (t nil))
    (or (and (string= fov "x")
             (fix= xvf 2)
             (fix= yvf 2))
        (and (string= fov "l")
             (fix= xvf 1)
             (fix= yvf 2))
        (and (string= fov "r")
             (fix= xvf 3)
             (fix= yvf 2))
        (and (string= fov "b")
             (fix= xvf 2)
             (fix= yvf 1))
        (and (string= fov "f")
             (fix= xvf 2)
             (fix= yvf 3)))))

;;; functions used by analyze to express microworld relationships continued
        
;; relate coarse visual field to detailed visual field via hand movement
(defun RELATIONSHIP-VF-HAND-FOV (vf hand fov)
  (let ((xvf (read-from-string (subseq vf 0 1)))
        (yvf (read-from-string (subseq vf 1 2))))
    (declare (fixnum xvf yvf))
    (cond ((string= hand "l") (setf xvf (fix1- xvf)))
          ((string= hand "r") (setf xvf (fix1+ xvf)))
          ((string= hand "b") (setf yvf (fix1- yvf)))
          ((string= hand "f") (setf yvf (fix1+ yvf)))
          (t nil))
    (or (and (string= fov "x")
             (fix= xvf 2)
             (fix= yvf 2))
        (and (string= fov "l")
             (fix= xvf 1)
             (fix= yvf 2))
        (and (string= fov "r")
             (fix= xvf 3)
             (fix= yvf 2))
        (and (string= fov "b")
             (fix= xvf 2)
             (fix= yvf 1))
        (and (string= fov "f")
             (fix= xvf 2)
             (fix= yvf 3)))))
        
;; relate detailed visual field to coarse visual field via hand movement
(defun RELATIONSHIP-FOV-HAND-VF (fov hand vf)
  (let ((xvf (read-from-string (subseq vf 0 1)))
        (yvf (read-from-string (subseq vf 1 2))))
    (declare (fixnum xvf yvf))
    (cond ((string= hand "l") (setf xvf (fix1+ xvf)))
          ((string= hand "r") (setf xvf (fix1- xvf)))
          ((string= hand "b") (setf yvf (fix1+ yvf)))
          ((string= hand "f") (setf yvf (fix1- yvf)))
          (t nil))
    (or (and (string= fov "x")
             (fix= xvf 2)
             (fix= yvf 2))
        (and (string= fov "l")
             (fix= xvf 1)
             (fix= yvf 2))
        (and (string= fov "r")
             (fix= xvf 3)
             (fix= yvf 2))
        (and (string= fov "b")
             (fix= xvf 2)
             (fix= yvf 1))
        (and (string= fov "f")
             (fix= xvf 2)
             (fix= yvf 3)))))

;;; functions used by analyze to express microworld relationships continued

;; express the relationship between gaze position, shift and the
;; coarse visual appearance of the body
;; in vp00 the body appears at vf31
;; in vp11 the body appears at vf20
;; so, the first digits must add to 3
;; the second digits must add to 1
;; modulo the eye movement
;; if "l", subtract 1 from xvp
;; if "r", add 1 to xvp
;; if "b", subtract 1 from yvp
;; if "f", add 1 to yvp
;; xvp and yvp range from 0-2 inclusive
(defun RELATIONSHIP-VP-EYE-VF (vp eye vf)
  (let ((xvp (read-from-string (subseq vp 0 1)))
        (yvp (read-from-string (subseq vp 1 2)))
        (xvf (read-from-string (subseq vf 0 1)))
        (yvf (read-from-string (subseq vf 1 2))))
    (declare (fixnum xvp yvp xvf yvf))
    (cond ((and (string= eye "l") (fix/= 0 xvp)) (setf xvp (fix1- xvp)))
          ((and (string= eye "r") (fix/= 2 xvp)) (setf xvp (fix1+ xvp)))
          ((and (string= eye "b") (fix/= 0 yvp)) (setf yvp (fix1- yvp)))
          ((and (string= eye "f") (fix/= 2 yvp)) (setf yvp (fix1+ yvp)))
          (t nil))
    (and (fix= (fix+ xvp xvf) 3)
         (fix= (fix+ yvp yvf) 1))))

;; express the relationship between gaze position, shift and the
;; detailed visual appearance of the body
;; same as above for calculating new vp position
;; then the foveal regions can be mapped easily to the body position
;; note that the body only appears in the back foveal region
(defun RELATIONSHIP-VP-EYE-FOV (vp eye fov)
  (let ((xvp (read-from-string (subseq vp 0 1)))
        (yvp (read-from-string (subseq vp 1 2))))
    (declare (fixnum xvp yvp))
    (cond ((and (string= eye "l") (fix/= 0 xvp)) (setf xvp (fix1- xvp)))
          ((and (string= eye "r") (fix/= 2 xvp)) (setf xvp (fix1+ xvp)))
          ((and (string= eye "b") (fix/= 0 yvp)) (setf yvp (fix1- yvp)))
          ((and (string= eye "f") (fix/= 2 yvp)) (setf yvp (fix1+ yvp)))
          (t nil))
    (and (string= fov "b")
             (fix= xvp 1)
             (fix= yvp 0))))

;;; functions used by analyze to express microworld relationships continued

;; relates detailed body visual details to visual position
;; body in b foveal position -> vp10
(defun RELATIONSHIP-FOV-EYE-VP (fov eye vp)
  (and (string= fov "b") ;vp10
       (or (and (string= eye "b")
                (string= vp "10"))
           (and (string= eye "f")
                (string= vp "11"))
           (and (string= eye "l")
                (string= vp "00"))
           (and (string= eye "r")
                (string= vp "20")))))

;; visual network
(defun RELATIONSHIP-VP-EYE-VP (vp1 eye vp2)
  (let ((x1 (read-from-string (subseq vp1 0 1)))
        (y1 (read-from-string (subseq vp1 1 2)))
        (x2 (read-from-string (subseq vp2 0 1)))
        (y2 (read-from-string (subseq vp2 1 2))))
    (declare (fixnum x1 y1 x2 y2))
    (or (and (string= vp1 vp2)
             (or (and (string= eye "b")
                      (fix= y1 0))
                 (and (string= eye "f")
                      (fix= y1 2))
                 (and (string= eye "l")
                      (fix= x1 0))
                 (and (string= eye "r")
                      (fix= x1 2))))
        (and (string= eye "l")
             (fix= x1 (fix1+ x2))
             (fix= y1 y2))
        (and (string= eye "r")
             (fix= (fix1+ x1) x2)
             (fix= y1 y2))
        (and (string= eye "b")
             (fix= x1 x2)
             (fix= y1 (fix1+ y2)))
        (and (string= eye "f")
             (fix= x1 x2)
             (fix= (fix1+ y1) y2)))))

;;; functions used by analyze to express microworld relationships continued

;; hand network
(defun RELATIONSHIP-HP-HAND-HP (hp1 hand hp2)
  (let ((x1 (read-from-string (subseq hp1 0 1)))
        (y1 (read-from-string (subseq hp1 1 2)))
        (x2 (read-from-string (subseq hp2 0 1)))
        (y2 (read-from-string (subseq hp2 1 2))))
    (declare (fixnum x1 y1 x2 y2))
    (or (and (string= hp1 hp2)
             (or (and (string= hand "b")
                      (fix= y1 0))
                 (and (string= hand "f")
                      (fix= y1 2))
                 (and (string= hand "l")
                      (fix= x1 0))
                 (and (string= hand "r")
                      (fix= x1 2))))
        (and (string= hand "l")
             (fix= x1 (fix1+ x2))
             (fix= y1 y2))
        (and (string= hand "r")
             (fix= (fix1+ x1) x2)
             (fix= y1 y2))
        (and (string= hand "b")
             (fix= x1 x2)
             (fix= y1 (fix1+ y2)))
        (and (string= hand "f")
             (fix= x1 x2)
             (fix= (fix1+ y1) y2)))))

;;; functions used by analyze to express microworld relationships continued

;; relates the current body position to a visual position
(defun RELATIONSHIP-VF-EYE-VP (vf eye vp)
  (let ((xvf (read-from-string (subseq vf 0 1)))
        (yvf (read-from-string (subseq vf 1 2)))
        (xvp (read-from-string (subseq vp 0 1)))
        (yvp (read-from-string (subseq vp 1 2))))
    (declare (fixnum xvp yvp xvf yvf))
    (cond ((string= eye "l") (setf xvf (fix1+ xvf)))
          ((string= eye "r") (setf xvf (fix1- xvf)))
          ((string= eye "b") (setf yvf (fix1+ yvf)))
          ((string= eye "f") (setf yvf (fix1- yvf)))
          (t nil))
    (or (and (fix= (fix+ xvp xvf) 3)
             (fix= (fix+ yvp yvf) 1))
        (and (string= eye "l")
             (fix= xvp 0)
             (fix= (fix+ yvp yvf) 1)
             (string= (subseq vf 0 1) "3"))
        (and (string= eye "r")
             (fix= xvp 2)
             (fix= (fix+ yvp yvf) 1)
             (string= (subseq vf 0 1) "1"))
        (and (string= eye "b")
             (fix= yvp 0)
             (fix= (fix+ xvp xvf) 3)
             (string= (subseq vf 1 2) "1")))))

;; positions where the hand will end up at hp10 in front of the body
(defun RELATIONSHIP-HP-HAND-BODY (hp hand)
  (or (and (string= hand "b")
           (or (string= hp "10")
               (string= hp "11")))
      (and (string= hand "r")
           (string= hp "00"))
      (and (string= hand "l")
           (string= hp "20"))))

;; positions where the hand moves from hp10 in front of the body
(defun RELATIONSHIP-BODY-HAND-HP (hand hp)
  (or (and (string= hand "b")
           (string= hp "10"))
      (and (string= hand "f")
           (string= hp "11"))
      (and (string= hand "r")
           (string= hp "20"))
      (and (string= hand "l")
           (string= hp "00"))))

;;; function used for generating the summary calculated data for each
;;; of the table series

(defun CALC (nums-with-ands)
    (let* ((num1 (delete '& nums-with-ands))
           (num2 (delete '\\ num1))
           (num3 (delete '\\& num2))
           (num4 (delete '\hline num3))
           (nums (delete '\hline& num4))
           (num (length nums))
           (sorted (sort nums #'<)))
      (format t "& ~D & ~D & ~D & ~F ~%"
              (first sorted)
              (nth (fix1- num) sorted)
              (nth (fix1- (floor (fix/ num 2))) sorted)
              (let ((total 0))
                (declare (fixnum total))
                (dolist
                 (x nums)
                 (setf total (fix+ total x)))
                (float (/ total num))))))
