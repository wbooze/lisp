;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; TEST DATA

(db-define blocks
  (class	name 	color	size	weight	position)
  (box		box1	black	medium	heavy	(2 6))
  (ball		ball1	blue	large	heavy	(4 6))
  (wedge	wedge1	gray	small	light	(6 6))
  (wedge	wedge2	gray	large	heavy	(8 6))
  (brick	brick1	blue	long	light	(4 2))
  (brick	brick2	black	long	light	(5 2))
  (brick	brick3	red	long	heavy	(6 2))
  (brick	brick4	red	short	light	(7 2)))

(db-define tools
  (class	name 		cost	size	supplier	quality)
  (saw		saw1		15	medium	sears		good)
  (hammer	hammer1		18	large	fly-by-night	bad)
  (wrench	wrench1		10	small	true-value	bad)
  (wrench	wrench2		12	large	sears		good)
  (screwdriver	screwdriver1	3	long	true-value	good)
  (screwdriver	screwdriver2	4	long	true-value	bad)
  (screwdriver	screwdriver3	4	long	fly-by-night	bad)
  (screwdriver	screwdriver4	2	short	true-value	good))



