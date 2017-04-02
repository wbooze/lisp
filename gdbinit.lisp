## SBCL debugging commands
## Copyright (C) 2003 andreas fuchs <asf@void.at>
## No warranty whatsoever (if applicable). 
## Do whatever you want with this code.

## Install by copying it to /path/to/sbcl-source/src/runtime/.gdbinit

## Usage:
## See http://sbcl-internals.cliki.net/gdb

## most of these functions are tested on x86 only. Please tell me
## about results you get when trying on other architectures.

define hookpost-stepi
y
  disasm_from
end
document hookpost-stepi
 workaround for gdb strangeness. it won't print the disassembled
 current insn, but a unusable line number in a nonexistent file.
end

define hookpost-nexti
y
  disasm_from
end
document hookpost-nexti
 workaround for gdb strangeness. it won't print the disassembled
 current insn, but a unusable line number in a nonexistent file.
end

set $SYM_NIL = 0

define initialize
y
  printf "finding symbol nil from "
  call lookup_by_name ("nil")
  set $SYM_NIL = ((struct code*) $)->header
  printf "nil found at value 0x%x\n", $SYM_NIL
end
document initialize
  initialize the value of $SYM_NIL to the real NIL symbol.
end


define hookpost-attach
y
  #initialize
end
document hookpost-attach
 call the initialization function
end

define hookpost-run
y
  initialize
end
document hookpost-run
 call the initialization function
end

set $N_LOWTAG_BITS = 3
set $LOWTAG_MASK = ((1<<$N_LOWTAG_BITS)-1) 

define y
end
document y
 convenience function: "y"es we want to redefine the command.
end

define id_lowtag
y
  printf "Lowtag of 0x%x: 0x%x (%s)\n", $arg0, $arg0 & 7, lowtag_Names[$arg0 & 7]
end
document id_lowtag 
  identify the lowtag of the first arg (a lisp pointer)
end

define xlisp
y
  # list lowtag
  if (($ & 0x3) == 0x3)
    print/x * ((struct cons*) ($ & ~$LOWTAG_MASK))
  # function lowtag
  else 
    if (($ & 0x5) == 0x5)
      printf "0x%x is a function.\n", $
      print/x * ((struct simple_fun *) ($ & ~$LOWTAG_MASK))
      printf "name of function follows:\n"
      x/8s $.name
    else 
      if (($ & 0x4) == 0x4)
	printf "0x%x is an (odd) fixnum: ", $
	print ($ >> ($N_LOWTAG_BITS-1))
      else
	if (($ & $LOWTAG_MASK) == 0)
	  printf "0x%x is an (even) fixnum: ", $
	  print ($ >> ($N_LOWTAG_BITS-1))
	else
	  printf "Unknown lowtag when printing 0x%x: 0x%x.\n", $, $ & $LOWTAG_MASK
	end
      end
    end
  end
end
document xlisp
  examine the last (or second-to-last) lisp object. 
  Currently, only functions and cons cells are supported.
end

define xlispa
y
  printf "examining "
  print/x $arg0
  xlisp
end
document xlispa
  examine the first argument with xlisp.
end


define xcar
y
  print/x $.car
  xlisp
end
document xcar
  examine the CAR of the last object (assuming it was of type
  struct cons).
end

define xcdr
y
  print/x $.cdr
  xlisp
end
document xcdr
  examine the CDR of the last object (assuming it was of type
  struct cons).
end

define find_lispfunction
y
  printf "searching dynamic space for address 0x%x: found ", $arg0
  call search_dynamic_space($arg0)
  printf "this object is probably a 'code' object...\n"
  print/x * ((struct code*) $)
  printf "trying 'entry_points' (i.e. a chain of functions): "
  print/x $.entry_points
  xlisp
  while $.next != $SYM_NIL
    $this_function = $
    printf "next function: "
    print/x $this_function.next
    xcar
    print/x $this_function
    xcdr
  end
end
document find_lispfunction
  find function names of objects of which we know only a $PC address.
  This function uses heuristics to find and display the entry points. 
end

define disasm_from
y
  disassemble $pc $pc+24
end
document disasm_from
  disassemble the block of memory starting from $pc, for lack of a
  useful disassembly routine.
end


set print pretty on

# antifuchs's debugging routine


define onwards
y
  set go=1
  cont
end

define spinattach
y
 while 1
	attach $arg0
 end
end


set args --core /home/asf/dl/cvs/sbcl.thread/output/cold-sbcl.core
handle SIGTRAP pass
handle SIGBUS pass
