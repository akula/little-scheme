(defun pudding-eater (person)
  (case person
	( (henry) (setf *arch-enemy* 'lisp-user)
	  '(fuck you lisp user))
	( (johnny) (setf *arch-enemy* 'no-functional-programming)
	  '(bitch all))
	(otherwise '(magoo-go-to-hell))))