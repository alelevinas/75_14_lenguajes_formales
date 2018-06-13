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

;asigna el valor "val" a la variable "var" en memoria "mem"
;Si no existe la variable devuelve ERROR_VARIABLE_NO_DECLARADA
(defun asignar (var val mem &optional acum)
	(if (null mem) `ERROR_VARIABLE_NO_DECLARADA
		(if (eq (car mem) var) (append acum (list var val) (cddr mem))
			(asignar var val (cddr mem) (append (list (car mem) (cadr mem)) acum))
		)
	)
)

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

(defun pertenece (x l)
	(if (null l) nil
		(if (eq x (car l)) T
			(pertenece x (cdr l))
		)
	)
)

;dada una operación del estilo x++, x--, x+= 8+x
;devuelve el último termino de la conversión a x=x+1 --> 1 o x=x+(8+x) --> (8+x)
(defun buscarresto (operador resto)
	(if (pertenece operador `(++ --)) 1
		(if (null (cdr resto)) (car resto)
			resto)
	)
)

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

;Recibe prg: las intrucciones del programa
;entr: el "pipe" de entrada std
;mem: la memoria que indica las variables declaradas y sus valores
;sal: el "pipe" de la salida std
(defun ejec (prg entr mem &optional (sal ()))
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
			((eq (caar prg) `DO)
				(ejec (append (nth 1 (car prg)) (cons (list 'WHILE (nth 3 (car prg)) (nth 1 (car prg)))  (cdr prg))) entr mem sal)
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


(defun verificar_simbolo (simbolo mem)
	(cond
		((listp simbolo) (reduce (lambda(x y) (and x y)) (mapcar (lambda(x)(verificar_simbolo x mem)) simbolo)))
		((pertenece simbolo `(CIN COUT IF ELSE WHILE DO = + - * / ++ -- += -= *= /= < > != ==)) T)
		((numberp simbolo) T)
		((stringp simbolo) T)
		((not (eq (buscar simbolo mem) `ERROR_VARIABLE_NO_DECLARADA)) T)
		(T nil)
	)
)

(defun verificar_variables (prg mem)
	(if (null prg) T
		(and (reduce (lambda(x y) (and x y)) (mapcar (lambda(x)(verificar_simbolo x mem)) (car prg))) (verificar_variables (cdr prg) mem))
	)
)


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