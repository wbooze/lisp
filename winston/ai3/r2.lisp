;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; Created: 23 July 1991
;;;; Purpose: Implement representations based on frames.

#+comment
(compile-file "/phw/modules/clos")

(require 'clos "/phw/modules/clos")

;;; TEST

(defparameter *test*
  nil
  "When T, evaluate arguments of TEST; when nil, ignore them.")

(defmacro test (&rest expressions)
  "
  Arguments:	Things to evaluate.
  Returns:	Nothing.
  "
  `(progn (if *test* (progn ,@expressions)
	    (format t "~%Test ignored."))
	  (values)))

;;; FRAME SYSTEM

#|
Purpose: Provide example of frame system with inheritance.
Remarks:
o Based on Common Lisp Object System (CLOS).
o Adapted from book example.
o When-created procedures are implemented with :initform.
o Views are not implemented.
|#

(defmacro make-frame (&rest classes)
  "
  Purpose:	Create instances.
  Remarks:	To allow for multiple superclasses, frames are
		connected to a nameless class, which is connected, in turn,
		to the multiple superclasses:
  "
  (let ((class (gentemp 'class-)))
    `(progn (defclass ,class ,classes ())
	    (make-instance ',class))))

(defclass dwarfs ()
  ((physique :initform 'fat :accessor physique)
   (appetite :initform 'medium :accessor appetite)
   (hobby :initform 'unknown :accessor hobby)
   (name :initform 'unknown :accessor name)))
(defclass eccentrics (dwarfs)
  ((personality :initform 'wierd :accessor personality)))
(defclass teachers (dwarfs) ())
(defclass programmers (dwarfs) ())
(defclass athletes (dwarfs) ())
(defclass endomorphs (dwarfs) ())
(defclass professors (eccentrics teachers) ())
(defclass hackers (eccentrics programmers)
  ((personality :initform 'shy :accessor personality)))
(defclass weightlifters (athletes endomorphs) ())
(defclass shotputters (athletes endomorths) ())

(defmethod hobby ((x athletes))
  "
  Purpose:	Forget about default hobby.  Athlete's exercise.
  Remarks:	When-requested procedures are implemented as ordinary methods.
		Shadows default procedure that just reads slots.
  "
  'exercise)

(defmethod hobby ((x dwarfs))
  "
  Purpose:	Forget about default hobby.
		If a dwarf is shy, he reads, otherwise he dances.
  Remarks:	Shadows default procedure that just reads slots.
  "
  (if (eq 'shy (personality x)) 'reading 'dancing))

(defmethod (setf physique) :after ((x athletes) y)
  "
  Purpose:	If an athlete is muscular, he eats a lot.
  Remarks:	When-written procedures are implemented as setf :after methods.
  "
  (when (eq 'muscular y) (setf (appetite x) 'large)))

(test
  (setf crazy (make-frame professors hackers))
  (setf jacque (make-frame weightlifters shotputters athletes))
  (format t "~%Crazy's hobby is ~a." (hobby crazy))
  (format t "~%Jacque's hobby is ~a." (hobby jacque))
  (format t "~%Jacque's physique is ~a." (physique jacque))
  (setf (physique jacque) 'muscular)
  (format t "~%Jacque's physique is ~a." (physique jacque))
  (format t "~%Jacque's appetite is ~a." (appetite jacque))
  (format t "~%Crazy's appetite is ~a." (appetite jacque))
  (show crazy)
  (show jacque))

;;; THEMATIC-ROLE SYSTEM

#|
Purpose: Provide example of trf system.
|#

(defclass trf ()
  ((agent :accessor agent)
   (verb :accessor verb)
   (thematic-object :accessor thematic-object)
   
   (coagent :accessor coagent)
   (beneficiary :accessor beneficiary)
   (instrument :accessor instrument)
   
   (source :accessor source)
   (destination :accessor destination)
   (trajectory :accessor trajectory)
   (conveyance :accessor conveyance)
   
   (location :accessor location)
   (time :accessor time)))

(test
  (setf trf (make-instance 'trf))
  (setf (verb trf) 'eat)
  (setf (instrument trf) 'spoon)
  (format t "~%The sample eating frame is described as:")
  (show trf))

;;; PRIMITIVE ACT SYSTEM

;;; VALUE PROPAGATION NET

;;; ARITHMETIC CONSTRAINT NET

#|
Purpose: Provide example of athrimetic constraint net propagation.
Remarks:
o Adaped from book example.
o Only adders are implemented to save space.
o Adders work in forward direction only.
o Wires are implemented as methods.
|#

(defclass terminal-box ()
  ((output :initform 'unknown :accessor output)))

(defclass two-terminal-box (terminal-box)
  ((input :initform 'unknown :accessor input)))

(defclass three-terminal-box (terminal-box)
  ((input1 :initform 'unknown :accessor input1)
   (input2 :initform 'unknown :accessor input2)))

(defclass junction ()
  ((terminals :initform nil :accessor terminals)))

(defclass port ()
  ((terminal :initform 'unknown :accessor terminal)))

(defclass adder (three-terminal-box) ())

(defmethod (setf input1) :after ((adder adder) input1)
  "
  Purpose:	Use new value on input1 terminal.
  "
  (let ((input2 (input2 adder))
	(output (output adder)))
    (cond ((numberp input2)
	   ;; Input2 has a value; change output:
	   (let ((sum (+ input1 input2)))
	     ;; Do nothing if unless there is a change:
	     (unless (and (numberp output) (eql output sum))
	       (format t "~%Setting OUTPUT of ~a to ~a." adder sum)
	       (setf (output adder) sum))))
	  #+comment
	  ((numberp output)
	   ;; Input2 has no value, but output does; change input2:
	   (let ((difference (- output input1)))
	     ;; Do nothing if unless there is a change:
	     (unless (eql input2 difference)
	       (format t "~%Setting INPUT2 of ~a to ~a." adder difference)
	       (setf (input2 adder) difference)))))))

(defmethod (setf input2) :after ((adder adder) input2)
  "
  Purpose:	Use new value on input2 terminal.
  "
  (let ((input1 (input1 adder))
	(output (output adder)))
    (cond ((numberp input1)
	   ;; Input1 has a value; change output:
	   (let ((sum (+ input1 input2)))
	     ;; Do nothing if unless there is a change:
	     (unless (and (numberp output) (eql output sum))
	       (format t "~%Setting OUTPUT of ~a to ~a." adder sum)
	       (setf (output adder) sum))))
	  #+comment
	  ((numberp output)
	   ;; Input1 has no value, but output does; change input1:
	   (let ((difference (- output input1)))
	     ;; Do nothing if unless there is a change:
	     (unless (eql input1 difference)
	       (format t "~%Setting INPUT1 of ~a to ~a." adder difference)
	       (setf (input1 adder) difference)))))))

(defmethod (setf output) :after ((adder adder) output)
  "
  Purpose:	Use new value on output terminal.
  "
  (let ((input1 (input1 adder))
	(input2 (input2 adder)))
    (cond ((and (numberp input1) (numberp input2))
	   ;; Input1 and input2 have values; check for consistency:
	   (unless (eql output (+ input1 input2))
	     (format t "New output of ~a is not compatible with its inputs!"
		     adder)))
	  #+comment
	  ((and (numberp input1) (not (numberp input2)))
	   ;; Only input1 has a value; give value to output2:
	   (format t "~%Setting INPUT2 of ~a to ~a." adder (- output input1))
	   (setf (input2 adder) (- output input1)))
	  #+comment
	  ((and (numberp input2) (not (numberp input1)))
	   ;; Only input2 has a value; give value to output1:
	   (format t "~%Setting INPUT1 of ~a to ~a." adder (- output input2))
	   (setf (input1 adder) (- output input2))))))

(defclass multiplier (three-terminal-box) ())

(defmethod (setf input1) :after ((multiplier multiplier) input1)
  "
  Purpose:	Use new value on input1 terminal.
  "
  (let ((input2 (input2 multiplier))
	(output (output multiplier)))
    (cond ((numberp input2)
	   ;; Input2 has a value; change output:
	   (let ((sum (* input1 input2)))
	     ;; Do nothing if unless there is a change:
	     (unless (and (numberp output) (eql output sum))
	       (format t "~%Setting OUTPUT of ~a to ~a." multiplier sum)
	       (setf (output multiplier) sum)))) )))

(defmethod (setf input2) :after ((multiplier multiplier) input2)
  "
  Purpose:	Use new value on input2 terminal.
  "
  (let ((input1 (input1 multiplier))
	(output (output multiplier)))
    (cond ((numberp input1)
	   ;; Input1 has a value; change output:
	   (let ((sum (* input1 input2)))
	     ;; Do nothing if unless there is a change:
	     (unless (and (numberp output) (eql output sum))
	       (format t "~%Setting OUTPUT of ~a to ~a." multiplier sum)
	       (setf (output multiplier) sum)))) )))

(defmethod (setf output) :after ((multiplier multiplier) output)
  "
  Purpose:	Use new value on output terminal.
  "
  (let ((input1 (input1 multiplier))
	(input2 (input2 multiplier)))
    (cond ((and (numberp input1) (numberp input2))
	   ;; Input1 and input2 have values; check for consistency:
	   (unless (eql output (* input1 input2))
	     (format t "New output of ~a is not compatible with its inputs!"
		     multiplier)))
	  #+comment
	  ((and (numberp input1) (not (numberp input2)))
	   ;; Only input1 has a value; give value to output2:
	   (format t "~%Setting INPUT2 of ~a to ~a."
		   multiplier (/ output input1))
	   (setf (input2 multiplier) (/ output input1)))
	  #+comment
	  ((and (numberp input2) (not (numberp input1)))
	   ;; Only input2 has a value; give value to output1:
	   (format t "~%Setting INPUT1 of ~a to ~a."
		   multiplier (/ output input2))
	   (setf (input1 multiplier) (/ output input2))))))

(defmacro wire (&rest args)
  "
  Purpose:	Define wire via when written method.
  Remarks:	This approach does not work when the number of ports varies.
  Example:	(wire terminal1 of object1 to terminal2 of object2).
		(wire terminal of object to junction)
  "
  (delete 'to (delete 'of args))
  (case (length args)
    (3 `(connect-to-junction ,@args))
    (4 `(connect-to-terminal ,@args))))

(defmacro connect-to-junction (terminal object junction)
  "
  Purpose:	Auxiliary.
  Remarks:	Tricky evaluation problem.
  "
  `(progn
     (pushnew (list ',terminal ,object) (terminals ,junction) :test #'equal)
     (defmethod (setf ,terminal) :after ((object (eql ,object)) value)
       (dolist (pair (terminals ,junction))
	 (let ((terminal (first pair)) (object (second pair)))
	   (unless (eql (eval `(,terminal object)) value)
	     (format t "~%Propagating ~a through ~a to ~a of ~a."
		     value j (first pair) (second pair))
	     (eval `(setf (,terminal object) value))))))))

(defmacro connect-to-terminal (terminal1 object1 terminal2 object2)
  "
  Purpose:	Auxiliary.
  "
  `(progn
      (defmethod (setf ,terminal1)
		 :after ((object (eql ,object1)) value)
	(unless (eql (,terminal2 ,object2) value)
	  (format t "~%Propagating ~a from ~a of ~a to ~a of ~a."
		  value ',terminal1 ,object1 ',terminal2 ,object2)
	  (setf (,terminal2 ,object2) value)))
      (defmethod (setf ,terminal2)
		 :after ((object (eql ,object2)) value)
	(unless (eql (,terminal1 ,object1) value)
	  (format t "~%Propagating ~a from ~a of ~a to ~a of ~a."
		  value ',terminal2 ,object2 ',terminal1 ,object1)
	  (setf (,terminal1 ,object1) value)))))

(test
  (setf a (make-instance 'multiplier))
  (setf b (make-instance 'multiplier))
  (setf j (make-instance 'junction))
  (setf p1 (make-instance 'port))
  (setf p2 (make-instance 'port))
  (setf p3 (make-instance 'port))
  (wire terminal of p1 to input1 of a)
  (wire terminal of p2 to j)
  (wire input2 of a  to j)
  (wire input2 of b  to j)
  (wire output of a to input1 of b)
  (wire output of b to terminal of p3)
  (setf (terminal p1) 3000)
  (setf (terminal p2) 1.1)
  (format t "~%Multiplier B's output is ~a." (terminal p3)))

;;; INFERENCE NET

;;; LABELED DRAWING

#|
Purpose: 
Remarks:
o This is work in progress; the class definitions are complete.
   Missing part is the part that enforces constraint.
   Also need slot for justification.
|#

(defclass thing ()
  ((interpretations accessor interpretations)))

(defclass line (thing)
  ((junction1 :initform 'unknown :accessor junction1)
   (junction2 :initform 'unknown :accessor junction2)
   (interpretations :initform '(+ - > <))))

(defclass J2 (thing)
  ((line1 :initform 'unknown :accessor line1)
   (line2 :initform 'unknown :accessor line2)))		;More!

(defclass J3 (thing)
  ((line1 :initform 'unknown :accessor line1)
   (line2 :initform 'unknown :accessor line2)
   (line3 :initform 'unknown :accessor line3)))

(defclass L (J2)
  ((interpretations :initform '((> >) (< <)))))

(defclass Fork (J3)
  ((interpretations :initform '((- - -) (+ + +)))))	;More!

(defmethod compute-line-labels ((l line)) (interpretations l))

(defmethod labels1 ((l l))
  (remove-duplicates (mapcar #'first (interpretations l))))
(defmethod labels2 ((l l))
  (remove-duplicates (mapcar #'second (interpretations l))))
(defmethod labels1 ((junction juction-with-3l))
  (remove-duplicates (mapcar #'first (interpretations junction))))
(defmethod labels2 ((junction juction-with-3l))
  (remove-duplicates (mapcar #'second (interpretations junction))))
(defmethod lables3 ((junction juction-with-3l))
  (remove-duplicates (mapcar #'third (interpretations junction))))

(defmethod (setf interpretations) ((line line))
  (setf (interpretations (junction1 line)) ---)
  (setf (interpretations (junction2 line)) ---))

(defmethod (setf interpretations) ((junction l))
  (setf (interpretations (line1 line)) ---)
  (setf (interpretations (line2 line)) ---))

(defmethod (setf interpretations) ((junction juction-with-3l))
  (setf (interpretations (line1 line)) ---)
  (setf (interpretations (line2 line)) ---)
  (setf (interpretations (line3 line)) ---))

;;; TIME NET

#|
Purpose: Provide example of time-net propagation.
Remarks:
o Not yet done; only defined classes so far.
o Will treat intervals a bit like junctions, I think.
|#

(defclass interval ()
  ((connections :initform nil :accessor connections)))

(defclass connection ()
  ((head :initform 'unknown :accessor head)
   (tail :initform 'unknown :accessor tail)
   (labels nil :accessor connection-labels)))

;;; TRUTH MAINTENANCE NET

#|
Purpose: Provide example of truth maintence.
Remarks:
o Adapted from book example.
o Only implication and negation implemented to save space.
o Uses classes and macros in definition of arithmetic constraint net.
|#

(defclass assertion (junction)
  "
  Purpose:	Defines a kind of junction.
  Remarks:	This makes it possible to see what is going on
		in the middle of a net; just hook a port to one of these.
  "
  ())

(defclass negates (two-terminal-box)
  "
  Purpose:	Defines negation constraint.
  "
  ())

(defclass implies (three-terminal-box)
  "
  Purpose:	Define implication constraint.
  Remarks:	Implication does not exist if INPUT2 is NIL.
  "
  ((input2 :initform t)))

(defmethod (setf input) :after ((box negates) input)
  "
  Purpose:	The output is the negation of the input.
  Remarks:	Looks for contradictions.
  "
  (let ((output (output box)))
    (if (t-or-nil-p output)
	(when (eq input output)
	  (format t "~%Contradiction noted in negation box ~a." box))
      (setf (output box) (not input)))))

(defmethod (setf output) :after ((box negates) output)
  "
  Purpose:	The input is the negation of the output.
  Remarks:	Looks for contradictions.
  "
  (let ((input (input box)))
    (if (t-or-nil-p input)
	(when (eq output input)
	  (format t "~%Contradiction noted in negation box ~a." box))
      (setf (input box) (not output)))))

(defmethod (setf input1) :after ((box implies) input1)
  "
  Purpose:	Output is T if input is T.
  Remarks:	Looks for contradictions.
  "
  (when (input2 box)
    (let ((output (output box)))
      (if (t-or-nil-p output)
	  (when (and input1 (not output))
	    (format t "~%Contradiction noted in implies box ~a." box))
	(when input1 (setf (output box) t))))))

(defmethod (setf output) :after ((box implies) output)
  "
  Remarks:	Just looks for contradictions.
  "
  (when (input2 box)
    (let ((input1 (input1 box)))
      (when (t-or-nil-p input1)
	(when (and input1 (not output))
	  (format t "~%Contradiction noted in implies box ~a." box))))))

(defun t-or-nil-p (x) (or (eql x t) (not x)))

(test
  (setf p1 (make-instance 'port))
  (setf p2 (make-instance 'port))
  (setf a1 (make-instance 'port))
  (setf a2 (make-instance 'port))
  (setf a3 (make-instance 'port))
  (setf j1 (make-instance 'assertion))
  (setf j2 (make-instance 'assertion))
  (setf j3 (make-instance 'assertion))
  (setf I1 (make-instance 'implies))
  (setf I2 (make-instance 'implies))
  (setf I3 (make-instance 'implies))
  (setf N  (make-instance 'negates))

  (wire input1 of I1 to terminal of P1)
  (wire output of I1 to J1)

  (wire input1 of I2 to J1)
  (wire output of I2 to J2)

  (wire input  of N to J2)
  (wire output of N to J3)

  (wire input1 of I3 to terminal of P2)
  (wire output of I3 to J3)

  (wire terminal of A1 to J1)
  (wire terminal of A2 to J2)
  (wire terminal of A3 to J3)

  (setf (terminal p1) t)
  (setf (terminal p2) t))

;;; NEURAL NET

#|
Purpose: Provide example of a neural net.
|#

(defclass neuron ()
  "
  Remarks:	Cannot use JUNCTION box because approximation-type
		neurons need to know where inputs come from.
  "
  ((input-synapses :initform nil :accessor input-synapses)
   (output-synapses :initform nil :accessor output-synapses)
   (neuron-output :initform 'unknown :accessor neuron-output)
   (total-inputs :initform 0 :accessor total-inputs)
   (inputs-received :initform 0 :accessor inputs-received)))

(defclass synapse ()
  ((synapse-input :initform 'unknown :accessor synapse-input)
   (synapse-output :initform 'unknown :accessor synapse-output)
   (output-neuron :initform 'unknown :accessor output-neuron)
   (weight :initform 1 :initarg :weight :accessor synapse-weight)))

(defmethod (setf synapse-input) :after ((synapse synapse) input)
  "
  Purpose:	Propagate input to output via multiplicative weight.
  "
  (setf (synapse-output synapse) (compute-synapse-output synapse)))

(defmethod compute-synapse-output ((synapse synapse))
  "
  Remarks:	May be shadowed for more specialized synapse classes.
  "
  (let ((output (* (synapse-input synapse) (synapse-weight synapse))))
    (format t "~%Setting output of synapse ~a to ~a." synapse output)
    output))

(defmethod (setf synapse-output) :after ((synapse synapse) ignore)
  "
  Purpose:	Compute neuron output if all inputs available:
  "
  (let* ((neuron (output-neuron synapse))
	 (inputs-received (inputs-received neuron))
	 (total-inputs (total-inputs neuron)))
    (setf (inputs-received neuron) (incf inputs-received))
    (when (= inputs-received total-inputs)
      (setf (inputs-received neuron) 0)	;Reset.
      (setf (neuron-output neuron)	;Compute output.
	    (compute-neuron-output neuron)))))

(defmethod compute-neuron-output ((neuron neuron))
  "
  Purpose:	Compute output value from output values of input synapses.
  Remarks:	May be shadowed for more specialized neuron classes.
  "
  (let ((output (threshold
		  (reduce #'+
			  (mapcar #'synapse-output (input-synapses neuron))))))
    (format t "~%Output of neuron ~a is ~a." neuron output)
    output))


(defmethod (setf neuron-output) :after ((neuron neuron) output)
  "
  Purpose:	Propagate neuron output to following synapses.
  "
  (dolist (w (output-synapses neuron)) (setf (synapse-input w) output)))

(defun threshold (sum) (if (plusp sum) 1 0))

(defmacro connect (&rest args)
  "
  Purpose:	Connect neurons via synapses.
  Example:	(connect <neuron> with <synapse> to <neuron>)
		(connect <synapse> to <neuron>)
  "
  (delete 'to (delete 'with args))
  `(case (length ',args)
     (2 (connect-synapse-output ,@args))
     (3 (progn (connect-synapse-output ,@(rest args))
	       (connect-synapse-input ,@(butlast args))))))

(defmacro connect-synapse-output (synapse neuron2)
  "
  Purpose:	Auxiliary.
  "
  `(progn
     (setf (input-synapses ,neuron2)
	   (attach-to-rear ,synapse (input-synapses ,neuron2)))
     (setf (total-inputs ,neuron2) (1+ (total-inputs ,neuron2)))
     (setf (output-neuron ,synapse) ,neuron2)))

(defmacro connect-synapse-input (neuron1 synapse)
  "
  Purpose:	Auxiliary.
  "
  `(progn
     (setf (output-synapses ,neuron1)
	   (attach-to-rear ,synapse (output-synapses ,neuron1)))))

(defun attach-to-rear (value list)
  "
  Purpose:	Splices new value on end of list.
  Remarks:	Needed to keep recorded input order same as connection order.
  "
  (let ((l (copy-list list)))
    (if l (setf (rest (last l)) (list value)) (setf l (list value)))
    l))

(test
  (setf n1 (make-instance 'neuron))
  (setf n2 (make-instance 'neuron))
  (setf n3 (make-instance 'neuron))
  (setf w1 (make-instance 'synapse))
  (setf w2 (make-instance 'synapse))
  (setf w13 (make-instance 'synapse :weight -.5))
  (setf w23 (make-instance 'synapse :weight .7))
  (connect n1 with w13 to n3)
  (connect n2 with w23 to n3)
  (connect w1 to n1)
  (connect w2 to n2)
  (setf (synapse-input w1) -.2)
  (setf (synapse-input w2) .3)
  (format t "~%Neuron n3's output is ~a." (neuron-output n3)))

;;; PERCEPTRON

#|
Purpose: Provide example of a perceptron.
|#

(defclass connection (synapse)
  "
  Purpose:	Connect perceptron to the world.
  Remarks:	A synapse that ignores its multiplier.  Really a wire.
  "
  ())

(defclass multiplier (synapse)
  "
  Purpose:	Multiply output of logic box times a weight.
  Remarks:	Just a synapse with a more conventional name.
  "  
  ((synapse-input :accessor multiplier-input)
   (weight :accessor multiplier-weight)))

(defclass logic-box (neuron)
  "
  Purpose:	Compute logical result from inputs when all available.
  Remarks:	Hitchhikes on ordinary neuron apparatus.
  "
  ())

(defclass threshold-box (neuron) ())

(defmethod compute-synapse-output ((synapse connection))
  "
  Remarks:	A connection is just a synapse that ignores its weight.
  "
  (let ((output (connection-input synapse)))
    (format t "~%Setting output of connection ~a to ~a." synapse output)
    output))

(defmacro define-logic-box (box expression)
  "
  Purpose:	Provides logic box's logic.
  Example:	(define-logic-box l1 (and (first inputs) (second inputs)))
  Remarks:	Note that inputs always refered to by position.
		Shadows ordinary neuron computation.
  "
  `(defmethod compute-neuron-output ((box (eql ,box)))
     (let* ((inputs (mapcar #'synapse-output (input-synapses box)))
	    (output ,expression))
       (format t "~%Output of logic box ~a is ~a." box output)
       (if output 1 0))))
           
(test
  (setf w1 (make-instance 'connection))
  (setf w2 (make-instance 'connection))
  (setf w3 (make-instance 'connection))
  (setf w4 (make-instance 'connection))
  (setf l1 (make-instance 'logic-box))
  (setf l2 (make-instance 'logic-box))
  (setf m1 (make-instance 'multiplier))
  (setf m2 (make-instance 'multiplier))
  (setf tb (make-instance 'threshold-box))
  (connect w1 to l1)
  (connect w2 to l1)
  (connect w3 to l2)
  (connect w4 to l2)
  (connect l1 with m1 to tb)
  (connect l2 with m2 to tb)
  (define-logic-box l1 (and (first inputs) (second inputs)))
  (define-logic-box l2 (or (first inputs) (second inputs)))
  (setf (multiplier-weight m1) .75)
  (setf (multiplier-weight m2) .25)
  (setf (synapse-input w1) t)
  (setf (synapse-input w2) nil)
  (setf (synapse-input w3) t)
  (setf (synapse-input w4) nil)
  (format t "~%Perceptron output is ~a." (neuron-output tb)))

;;; INTERPOLATION NET

(defclass adder-neuron (neuron) ())

(defmethod compute-neuron-output ((neuron adder-neuron))
  "
  Purpose:	Replace ordinary neuron computation.
  "
  (let ((inputs (mapcar #'synapse-output (input-synapses neuron))))
    (format t "~%Computing output for neuron ~a: ~a" neuron inputs)
    (setf (neuron-output neuron)
	  (reduce #'+ inputs))))

(defclass distance-neuron (neuron)
  ((samples :initarg :samples :accessor samples)))

(defmethod compute-neuron-output ((neuron distance-neuron))
  "
  Purpose:	Replace ordinary neuron computation.
  "
  (let ((samples (samples neuron))
	(inputs (mapcar #'synapse-output (input-synapses neuron))))
    (format t "~%Computing sample distance for neuron ~a: ~a ~a"
	    neuron samples inputs)
    (gaussian
      (reduce #'+
	      (mapcar #'(lambda (s i) (expt (- s i) 2))
		      samples
		      inputs)))))

(setf *sigma* 1.0)

(defun gaussian (squared-sum) (exp (* (/ -1.0 (* 2.0 *sigma*)) squared-sum)))

(test
  (setf nA (make-instance 'distance-neuron :samples '(0 0)))
  (setf nB (make-instance 'distance-neuron :samples '(1 1)))
  (setf nC (make-instance 'adder-neuron))
  (setf w1 (make-instance 'synapse))
  (setf w2 (make-instance 'synapse))
  (setf w3 (make-instance 'synapse))
  (setf w4 (make-instance 'synapse))
  (setf wAC (make-instance 'synapse :weight -.5))
  (setf wBC (make-instance 'synapse :weight .7))
  (connect w1 to nA)
  (connect w2 to nA)
  (connect w3 to nB)
  (connect w4 to nB)
  (connect nA with wAC to nC)
  (connect nB with wBC to nC)
  (setf (synapse-input w1) .25)
  (setf (synapse-input w2) .50)
  (setf (synapse-input w3) .75)
  (setf (synapse-input w4) 1.0)
  (format t "~%Output is ~a." (neuron-output nC))
  (setf (synapse-input w1) 0)
  (setf (synapse-input w2) 0)
  (setf (synapse-input w3) 1)
  (setf (synapse-input w4) 1)
  (format t "~%Output is ~a." (neuron-output nC)))

;;; APPROXIMATION NET





