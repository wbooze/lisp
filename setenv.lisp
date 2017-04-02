;;; Put code to massage *features* list here...
;;; This is read early in the build process so don't include complicated
;;; things there.  pushnew, setf, remove, are ok.  In particular, reader
;;; conditionals aren't supported.
;;;
;;; Most of these don't need to be set explicitly anymore because Lisp
;;; will set them based on the features that exist in the Lisp used to
;;; compile.  So, unless you're changing the set of features in the
;;; lisp you're using to compile from, there's probably nothing that
;;; needs to be uncommented here.
(in-package :cl-user)

;; Specific features that most people want:

(pushnew :hash-new *features*)
;;(pushnew :random-mt19937 *features*)
(pushnew :conservative-float-type *features*)
;;(pushnew :relative-package-names *features*)

;; Version tags

;;(pushnew :cmu19a *features*)
;;(pushnew :cmu19 *features*)
;;(setf *features* (remove :cmu17 *features*))
;;(setf *features* (remove :cmu18c *features*))
;;(setf *features* (remove :cmu18d *features*))
;;(setf *features* (remove :cmu18e *features*))

;; e.g. for Linux on x86 you probably want:
(pushnew :x86 *features*)
(pushnew :linux *features*)
;; Note! If you are still running glibc 2.1, you need the following:
;; (pushnew :glibc2.1 *features*)
;; Otherwise, i.e. for glibc 2.2 and later you want:
(setf *features* (remove :glibc2.1 *features*))

;; Select conservative GC:
;;(pushnew @@gcname@@ *features*)
(pushnew :gencgc *features*)
;; This X86 port supports multiprocessing and linkage-table
(pushnew :mp *features*)
(pushnew :linkage-table *features*)

;; CPU selection:
;; i486 gives you faster locking
(pushnew :i486 *features*)
;; Pentium gives you CPU cyclecounts in time
(pushnew :pentium *features*)
