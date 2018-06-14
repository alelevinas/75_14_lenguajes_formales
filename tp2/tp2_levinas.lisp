;Busca el valor de x en el ambiente
(defun buscar (x amb)
	(if (or (null x) (null amb)) nil
		(if (eq x (car amb)) (nth 1 amb)
			(buscar x (cdr (cdr amb)))
		)
	)
)

;Aplica la funcion fn con parametros lae (ya evaluados) con el ambiente amb
(defun aplicar (fn lae amb)
	(if (atom fn) 
		(cond
			((eq fn `+) (+ (car lae) (cadr lae)))
			((eq fn `-) (- (car lae) (cadr lae)))
			((eq fn `*) (* (car lae) (cadr lae)))
			((eq fn `/) (/ (car lae) (cadr lae)))
			((eq fn `CAR) (caar lae))
			((eq fn `CDR) (cdar lae))
			((eq fn `NUMBERP) (numberp (car lae)))
			((eq fn `EQ) (eq (car lae) (cadr lae)))
			((eq fn `NULL) (null (car lae)))
			((eq fn `CONS) (cons (car lae) (cadr lae)))
			((eq fn `LIST) lae)
			(T (aplicar (buscar fn amb) lae amb))
		)
	; caso lambda
	(evaluar (nth 2 fn) (aplicarenamb (nth 1 fn) lae amb))
	)
)

;Agrega las etiquetas con sus valores al ambiente
(defun aplicarenamb (vars vals amb)
	(append (reduce `append (mapcar `list vars vals)) amb)
)

;recibe una lista de listas y devuelve los pares 1-1 2-2 3-3, etc.
(defun emparejar (L)
	(if (null (car L)) nil
		(cons (mapcar 'car L) (emparejar (mapcar 'cdr L)))
	)
)

;(print (emparejar `((1 2 3))))

;Eval lisp de 1 ambiente
(defun evaluar (expr amb)
	(if (atom expr) 
		(cond ((null expr) nil)
			((numberp expr) expr)
			((eq expr `t) T)	
			(T (buscar expr amb))  ; buscar en el ambiente el valor de la variable
		)
		(cond 
			((eq (car expr) `QUOTE) (cadr expr))
			((eq (car expr) `OR)
				(if (evaluar (cadr expr) amb) T
					(evaluar (caddr expr) amb)))
			((eq (car expr) `AND) 
				(if (evaluar (cadr expr) amb)
					(if (evaluar (caddr expr) amb) T)))
			((eq (car expr) `LAMBDA) expr)
			((eq (car expr) `IF) 
				(if (evaluar (cadr expr) amb)
					(evaluar (caddr expr) amb)
					(evaluar (cadddr expr) amb)))
			((eq (car expr) `MAPCAR)
				(mapcar (lambda(param) (aplicar (evaluar (nth 1 expr) amb) param amb)) (emparejar (mapcar (lambda(x) (evaluar x amb)) (cddr expr)))))
			(T (aplicar (car expr) (mapcar (lambda (x) (evaluar x amb)) (cdr expr)) amb))
		)
	)
)

;(print (evaluar '((lambda (x) (* x 2)) (quote 2)) nil )) ; --> 4

;(print (evaluar '(mapcar (lambda (x) (* x 2)) (quote (2 3 4))) nil)) ; --> (4 6 8)
;(print (evaluar '(mapcar (lambda (x) (cons x (cdr (quote (3 4 5))))) (quote (1 2 3))) nil)) ; --> ((1 4 5) (2 4 5) (3 4 5))
;(print (evaluar '(mapcar numberp (quote (4))) nil)) ; --> (t)
;(print (evaluar '(mapcar numberp (quote (4 5 6))) nil)) ; --> (t t t)
;(print (evaluar '(mapcar 'car (quote ((2 3) (4 5)))) nil)) ; --> (2 4)
;(print (evaluar '(mapcar 'list (quote((cdr (quote (a b c))) 2 3))) nil)) ; --> (((CDR '(A B C))) (2) (3))
;(print (evaluar `(mapcar 'list '((cdr '(a b c)) 2 3)  '(m p q) '(x y z)) nil))

;(print (aplicarenamb `(x) `(3) ())); --> (x 3)
;(print (aplicarenamb `(x y) `(3 9) ())); --> (x 3 y 9)
;(print (aplicarenamb `(x y) `(3 9) `(k 9 p (1 2 3)))); --> (x 3 y 9 k 9 p (1 2 3))


;(print (evaluar `2 nil)) ; --> 2
;(print (evaluar nil nil)) ; --> nil
;(print (evaluar 't nil)) ; --> T
;(print (evaluar 'A '(A 99))) ; --> 99
;(print (evaluar 'B '(A 2 B 10))) ; --> 10
;(print (evaluar '(quote A) nil)) ; --> A
;(print (evaluar '(quote (A)) nil)) ; --> (A)
;(print (evaluar '(quote ((A B) ( C D))) nil)) ; --> (A)
;(print (evaluar '(quote 1) nil)) ; --> 1
;(print (evaluar '(quote (car a)) nil )) ; --> (car a)
;(print (evaluar '(quote ((2 3) (4 5))) nil)) ; --> ((2 3) (4 5))
;(print (evaluar '(and (or t nil) t) nil )) ; --> T
;(print (evaluar '(and (or t nil) (or nil nil)) nil)) ; --> nil
;(print (evaluar '(or (or t nil) (or nil nil )) nil)) ; --> T
;(print (evaluar '(car (list a 2 3)) '(a 100) )) ; --> 100
;(print (evaluar '(cdr (list a b c)) '(a 100 b 99 c 98) )) ; --> (99 98)
;(print (evaluar '((lambda (x) (* x 2)) 2) nil )) ; --> 4
;(print (evaluar '((lambda (x y) (+ (* x 2) y)) 2 4) nil)) ; --> 8
;(print (evaluar '(lambda (x) (* x 2)) nil)) ; --> (lambda (x) (* x 2))
;(print (evaluar '(mapcar (lambda (x) (* x 2)) (quote (2 3 4))) nil)) ; --> (4)
;(print (evaluar '(mapcar (lambda (x) (cons x (cdr (quote (3 4 5))))) (quote (1 2 3))) nil)) ; --> ((1 4 5) (2 4 5) (3 4 5))
;(print (evaluar '(mapcar numberp (quote (4))) nil)) ; --> (t)
;(print (evaluar '(mapcar numberp (quote (4 5 6))) nil)) ; --> (t t t)
;(print (evaluar '(mapcar car (quote ((quote(2 3)) (quote(4 5))))) nil)) ; --> (2 4)
;(print (evaluar '(fact 5) '(fact (lambda(n) (if (eq n 0) 1 (* n (fact (- n 1)))))))) ; --> 120
;(print (evaluar '(mapcar fact (quote (2 3 4 5))) '(fact (lambda(n) (if (eq n 0) 1 (* n (fact (- n 1)))))))) ; --> (2 6 24 120)

;(print (evaluar '(mapcar list (quote (4))) nil)) ; --> (t)