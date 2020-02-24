
(remove-methods 'personality)

(remove-methods 'setf-personality)

(symbol-function 'personality)

(symbol-function 'setf-personality)

(setf (physique jacque) 'muscular)

(format t "~%Jacque's personality is ~a." (personality jacque))

(untrace)

(untrace personality)

(trace personality process-methods fetch-methods)

(trace setf-personality)

(symbol-plist 'personality)
(SETF SETF-PERSONALITY :PRIMARY
      (((HACKERS) #<LEXICAL CLOSURE 15C:855>) ((ECCENTRICS) #<LEXICAL CLOSURE 164:4E9C>) ((DWARFS) #<LEXICAL CLOSURE 164:9B76>)))

(symbol-plist 'setf-personality)
(:PRIMARY
 (((HACKERS T) #<LEXICAL CLOSURE 1C4:B6FD>)
  ((ECCENTRICS T) #<LEXICAL CLOSURE 1D4:7A07>)
  ((DWARFS T) #<LEXICAL CLOSURE 18C:8B80>)))

(symbol-plist 'personality)
(SETF SETF-PERSONALITY :PRIMARY
      (((HACKERS) #<LEXICAL CLOSURE 1C4:BC1F>) ((ECCENTRICS) #<LEXICAL CLOSURE 1D4:7F5F>) ((DWARFS) #<LEXICAL CLOSURE 18C:9129>)))
(:AFTER NIL :BEFORE NIL SETF SETF-PHYSIQUE :PRIMARY
 (((COMPETITORS) #<LEXICAL CLOSURE 1BC:9333>)
  ((DWARFS) #<LEXICAL CLOSURE 1D4:E8BC>)))

(symbol-plist jacque)
(PERSONALITY GREGARIOUS PHYSIQUE FAT APPETITE SMALL IS-A JACQUES) 
 

(untrace applicable-p)

(trace make-precedence-list get-argument-class make-relatives-list)

(setf (physique jacque) 'muscular)

(physique jacque)

(remove-methods 'physique)