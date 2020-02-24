;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

(db-define beach1
	   (hair	height	weight	lotion	result)
	   (blond	tall	light	no	t)
	   (blond	tall	average	yes	t)
	   (blond	short	average	no	nil)
	   (blond	short	light	yes	nil))

(db-define beach2
	   (hair	height	weight	lotion	result)
	   (red		tall	heavy	no	nil)
	   (brown	tall	heavy	no	nil))