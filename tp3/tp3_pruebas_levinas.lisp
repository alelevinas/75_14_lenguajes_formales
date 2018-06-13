;Agrega las etiquetas con sus valores al ambiente
;Si no tiene valor, se le asigna 0
(defun agregarvar (vars mem)
	(if (null vars) mem
		(if (eq (nth 1 vars) `=)
			(agregarvar (cdddr vars) (append (list (car vars) (nth 2 vars)) mem))
			(agregarvar (cdr vars) (append (list (car vars) 0) mem))
		)
	)
)

`(`_________PRUEBAS_AGREGARVAR_________)
(agregarvar `(x y) nil); --> (y 0 x 0)
(agregarvar `(x = 2 y) nil); --> (y 0 x 2)
(agregarvar `(x = 2 y) `(z 9)); --> (y 0 x 2 z 9)

;asigna el valor "val" a la variable "var" en memoria "mem"
;Si no existe la variable devuelve ERROR_VARIABLE_NO_DECLARADA
(defun asignar (var val mem &optional acum)
	(if (null mem) `ERROR_VARIABLE_NO_DECLARADA
		(if (eq (car mem) var) (append acum (list var val) (cddr mem))
			(asignar var val (cddr mem) (append (list (car mem) (cadr mem)) acum))
		)
	)
)

`(`_________PRUEBAS_ASIGNAR_________)
(asignar `x 2 nil); --> ERROR_VARIABLE_NO_DECLARADA
(asignar `x 2 `(x 0)); --> (x 2)
(asignar `x 3 `(x 4)); --> (x 3)
(asignar `z 1 `(x 8 y 9)); --> ERROR_VARIABLE_NO_DECLARADA
(asignar `z 1 `(x 8 y 9 z nil p (1 2 3))); --> `(y 9 x 8 z 1 P (1 2 3))

(defun buscar (var mem)
	(if (null mem) `ERROR_VARIABLE_NO_DECLARADA
		(if (eq (car mem) var) (cadr mem)
			(buscar var (cddr mem))
		)
	)
)

(defun buscaroperador (operador)
	(cond
		((eq operador `++) `+)
		((eq operador `+=) `+)
		((eq operador `--) `-)
		((eq operador `-=) `-)
		((eq operador `*=) `*)
		((eq operador `/=) `/)
	)
)

`(`_________PRUEBAS_BUSCAROPERADOR_________)
(buscaroperador `++) ;--> +
(buscaroperador `/=) ;--> /

(defun pertenece (x l)
	(if (null l) nil
		(if (eq x (car l)) T
			(pertenece x (cdr l))
		)
	)
)

`(`_________PRUEBAS_PERTENECE_________)
(pertenece 8 `(9 7 12 8)) ; --> T
(pertenece 1 `(9 7 12 8)) ; --> nil

;dada una operación del estilo x++, x--, x+= 8+x
;devuelve el último termino de la conversión a x=x+1 --> 1 o x=x+(8+x) --> (8+x)
(defun buscarresto (operador resto)
	(if (pertenece operador `(++ --)) 1
		(if (null (cdr resto)) (car resto)
			resto)
	)
)

`(`_________PRUEBAS_BUSCARRESTO_________)
(buscarresto `++ nil) ;--> 1
(buscarresto `+= `(2 + 2)) ;--> (2 + 2)
(buscarresto `+= `(3)) ;--> 3

(defun bool (x)
	(if (null x) 0 1)
)

(defun operar (operador operando1 operando2)
	(cond
		((eq operador `!=) (bool (not (= operando1 operando2))))
		((eq operador `==) (bool (= operando1 operando2)))
		((eq operador `>) (bool (> operando1 operando2)))
		((eq operador `<) (bool (< operando1 operando2)))
		(T (reduce operador (list operando1 operando2)))
	)
)

`(`_________PRUEBAS_OPERAR_________)
(operar `+ 2 3); --> 5
(operar `< 2 3); --> 1
(operar `* 2 3); --> 6
(operar `/ 2 3); --> 2/3
(operar `== 2 3); --> 0
(operar `!= 2 3); --> 1

(defun peso (operador)
	(cond 
		((eq operador `+) 10)
		((eq operador `-) 10)
		((eq operador `*) 20)
		((eq operador `/) 20)
		((eq operador `<) 5)
		((eq operador `>) 5)
		((eq operador `==) 1)
		((eq operador `!=) 1)
		(T `OPERADOR_NO_DEFINIDO)
	)
)

(defun esoperador (operador)
	(pertenece operador `(+ - * / < > == !=))
)
;dada una operación x + 2 | x + 3 * 4 | (x + 3) * 4
;da su resultado 
(defun valor (expr mem &optional (operadores ()) (operandos ()))
	(if (null expr)
		(if (null operadores) (car operandos)
			(valor expr mem (cdr operadores) (cons (operar (car operadores) (cadr operandos) (car operandos))
												   (cddr operandos))
			)
		)
		(if (atom expr)
			(if (numberp expr) expr
				(buscar expr mem)
			)
			(if (not (esoperador (car expr)))
				(valor (cdr expr) mem operadores (cons (valor (car expr) mem) operandos))
				
				(if (null operadores) (valor (cdr expr) mem (list (car expr)) operandos) ;lista de operadores vacia
					(if (< (peso (car operadores)) (peso (car expr))) (valor (cdr expr) mem (cons (car expr) operadores) operandos)
						(valor expr mem (cdr operadores) (cons (operar (car operadores) (cadr operandos) (car operandos)) (cddr operandos)))
					)
				)
			)
		)
	)
)
`(`_________PRUEBAS_VALOR_________)
(valor `(2 + 2) nil) ; --> 4
(valor `(2 - 2) nil) ; --> 0
(valor `(2 * 2) nil) ; --> 4
(valor `(2 / 2) nil) ; --> 1
(valor `(2 == 2) nil) ; --> 1
(valor `(2 != 2) nil) ; --> 0
(valor `(2 > 2) nil) ; --> 0
(valor `(2 < 2) nil) ; --> 0
(valor `(2 * 3 + 4) nil) ; --> 10
(valor `(2 + 3 * 4) nil) ; --> 14
(valor `(2 * 3 + 4 * 4) nil) ; --> 22
(valor `(2 + x * 4) `(x 3)) ; --> 14
(valor `(2 > 3 == 9 < 5) `(x 3)) ; --> 1


(defun verificar_simbolo (simbolo mem)
	(cond
		((listp simbolo) (reduce (lambda(x y) (and x y)) (mapcar (lambda(x)(verificar_simbolo x mem)) simbolo)))
		((pertenece simbolo `(CIN COUT IF ELSE WHILE = + - * / ++ -- += -= *= /= < > != ==)) T)
		((numberp simbolo) T)
		((stringp simbolo) T)
		((not (eq (buscar simbolo mem) `ERROR_VARIABLE_NO_DECLARADA)) T)
		(T nil)
	)
)

(verificar_simbolo `IF nil)
(verificar_simbolo `2 nil)
(verificar_simbolo `a nil)
(verificar_simbolo `(x == 2) `(x 0))

(defun verificar_variables (prg mem)
	(if (null prg) T
		(and (reduce (lambda(x y) (and x y)) (mapcar (lambda(x)(verificar_simbolo x mem)) (car prg))) (verificar_variables (cdr prg) mem))
	)
)

;Recibe prg: las intrucciones del programa
;entr: el "pipe" de entrada std
;mem: la memoria que indica las variables declaradas y sus valores
;sal: el "pipe" de la salida std
(defun ejec (prg entr mem &optional (sal ()))
	;mem)
	;(car prg))
	(if (null prg) (reverse sal)
		(cond
			((null (car prg)) ; vengo de un if sin else
				(ejec (cdr prg) entr mem sal))
			((eq (caar prg) `CIN)
				(ejec (cdr prg) (cdr entr) (asignar (nth 1 (car prg)) (car entr) mem) sal))
			((eq (caar prg) `COUT)
				(ejec (cdr prg) entr mem (cons (valor (cdar prg) mem) sal)))
			((eq (nth 1 (car prg)) `=)
				(ejec (cdr prg) entr (asignar (caar prg) (valor (cddar prg) mem) mem) sal))
			((eq (caar prg) `IF)
				(if (eq (valor (cadar prg) mem) 1) ; si es true
					(ejec (append (nth 2 (car prg)) (cdr prg)) entr mem sal)
					(ejec (append (nth 4 (car prg)) (cdr prg)) entr mem sal)
				)
			)
			((eq (caar prg) `WHILE)
				(if (eq (valor (cadar prg) mem) 1) ; si es true, le vuelvo a poner todo el bloque while despues del cuerpo del while
					(ejec (append (nth 2 (car prg)) prg) entr mem sal)
					(ejec (cdr prg) entr mem sal))
			)
			((pertenece (caar prg) `(++ --))
				(ejec (cons (reverse (car prg)) (cdr prg)) entr mem sal))
			((pertenece (nth 1 (car prg)) `(++ -- += -= *= /=))
				(ejec 
					(cons (list (caar prg) `= (caar prg) (buscaroperador (nth 1 (car prg))) (buscarresto (nth 1 (car prg)) (cddar prg))) (cdr prg))
					 entr mem sal))
			(T `NO_EXISTE_LA_FUNCION_U_OPERADOR)
		)
	)
)

`(`_________PRUEBAS_EJEC_________)
(ejec `((CIN x) (COUT x)) `(2) `(x 0)); --> (2)
(ejec `((CIN x) (x += 2) (COUT x)) `(2) `(x 0)); --> (4)
(ejec `((x = 3) (COUT x)) nil `(x 0)); --> (3)
(ejec `((x = 3) (COUT (x == 3))) nil `(x 0)); --> (1)
(ejec `((x = 3) (COUT (x * 3 + 2))) nil `(x 0)); --> (11)
(ejec `((x = 3) (++ x) (COUT (x * 3 + 2))) nil `(x 0)); --> (14)
(ejec `((x = 4) (x ++) (COUT (x * 3 + 2))) nil `(x 0)); --> (17)
(ejec `((x = 4 * 3 + 2 / 2) (x ++) (COUT (x * 3 + 2))) nil `(x 0)); --> (44)
(ejec `((COUT x)) nil nil); --> (ERROR_VARIABLE_NO_DECLARADA)
(ejec `((CIN x) (x = x * n) (COUT x)) `(5) `(x 0 n 4)); --> (20)

;Crea la memoria del programa segun las variables declaradas
;hasta encontrarse con la función main
(defun run (prg entr &optional (mem ()))
	(if (null prg) nil
		(if (eq (caar prg) `INT)
			(run (cdr prg) entr (agregarvar (cdar prg) mem))
			(if (eq (caar prg) `MAIN)
				(if (verificar_variables (nth 1 (car prg)) mem) (ejec (nth 1 (car prg)) entr mem)
					'ERROR_VARIABLE_NO_DECLARADA
				)
			)
		)
	)
)

`(`_________PRUEBAS_RUN_________)
(RUN `( (int a = 2 b = 3)
          		(main (
					(cout a)
				)
				)
			) () )
 ; --> (2)
(RUN `( (int z = 2)
          (main (
			(cout b) )
			)
			) () )
 ; --> (ERROR_VARIABLE_NO_DECLARADA)
(RUN `( (int a = 6)
          (main (
                 (if (a == 2)
                     ( (cout (a + 1)))
				 )
				)
			)
			) () )
 ; --> NIL
(RUN `( (int a = 6)
          (main (
                 (if (a == 2)
                     ( (cout (a + 1) ) )
                  else
                  	 ( (cout (a - 1) ) )
				 )
				)
			)
			) () )
 ; --> (5)
(RUN `( (int a = 2)
          (main (
                 (if (a == 2)
                     ( (cout (a + 1) )
				) )
			) )
        ) () )
; --> (3)
(RUN `( (int a = 2 b)
          (main (
                 (cin b)
                 (a = b + 3)
                 (cout a)
				)
          )
  ) `(5) )
; --> (8)
(RUN `( (int a = 2 b)
          (main (
                 (a = (a + 1) * 4)
                 (b -= 5)
                 (a += 3)
                 (cout a)
                 (cin a)
                 (cout a)
                 (cout b)
				)
          )
       ) `(6) )
; --> (15 6 -5)

(RUN `( (int n fact = 1) 
			(main (
                 (cin n)
                 (if (n < 0 )
                     ( (cout "no existe fact de nro negativo" ) )
                  else
					(
					 (while (n > 1)
					  (
					  	(fact = fact * n)
					    (n -- )
					  )
					 )
					(cout fact )
				  )
				)
			)
		)
) `(5))
; --> (120)

(RUN `( 
	      (int x y p = 10)
          (int r)
          (main ( (x = p + 10)
                  (p ++)
                  (++ x)
                  (x *= p - 4)
                  (if (x < p)
                   ( (cout x + p)
                     (cin y)
                   )
                   else
                   ( (x = x * 6)
                     (cout p * p)
                   )
                  )
                  (while (x > p * 10)
                   (
                    (cout x + p)
                    (cin y)
                    (cout y)
                    (x -= y)
					)
                   )
				)
          )
) '(700 100))
; --> (121 893 700 193 100)

(RUN '( (int x y p = 10)                              
				(int r)
				(main (
					(x = p + 10)                             
					(p ++)
					(++ x)
					(x *= p - 4)
					(if (x > p)(
						(cout xj + p)
						)
					)
				)
				)
) '(1 2 3))