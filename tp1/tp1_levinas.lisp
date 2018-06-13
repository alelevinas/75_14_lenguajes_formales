(setq grafo `(
	(a (b f))
	(b (a c))
	(c (b d))
	(d (c n e))
	(e (d))
	(f (g))
	(g (h))
	(h (i l))
	(i (m j))
	(j (k))
	(k (o))
	(l (b f))
	(m (l c))
	(n (j m))
	(o (e n))
))

(setq diccionario `(
	(a (PaseoColon Independencia))
	(b (PaseoColon Chile))
	(c (PaseoColon Mexico ))
	(d (PaseoColon Venezuela))
	(e (PaseoColon Belgrano))
	(f (Independencia Balcarce))
	(g (Independencia Defensa))
	(h (Defensa Chile))
	(i (Defensa Mexico))
	(j (Defensa Venezuela))
	(k (Defensa Belgrano))
	(l (Balcarce Chile))
	(m (Balcarce Mexico))
	(n (Balcarce Venezuela))
	(o (Balcarce Belgrano))
))

(defun pertenece (x l)
	(if (null l) nil
		(if (eq x (car l)) T
			(pertenece x (cdr l))
		)
	)
)

;Solo recibe listas de atomos
;Devuelve los atomos de l1 que no estan en l2
(defun diff (l1 l2 &optional res)
	(cond ((null l1) (reverse res))
		((pertenece (car l1) l2) (diff (cdr l1) l2 res))
		(T (diff (cdr l1) l2 (cons (car l1) res)))
	)
)

;Recibe un nodo y un grafo como una lista de listas
;Cada sublista contiene un nodo y una sublista con los nodos vecinos
;Devuelve la sublista de vecinos del nodo recibido
;EJ grafo ((a (b c)) (b (a e d)) (c (a d e)) (d (b c)) (e (b c f)) (f (e)))
(defun vecinos (nodo grafo)
	(if (or (null nodo) (null grafo)) nil
		(if (eq nodo (caar grafo)) (cadar grafo)
			(vecinos nodo (cdr grafo))
		)
	)
)


;Recibe un nodo inicio, un nodo final y un grafo que los contiene
;Devuelve el primer camino que encuentra desde el nodo i hasta el nodo f
(defun camino (i f grafo &optional (tray (list (list i))))
	(print "--------------------")
	(print i)
	(print f)
	(print tray)
	(if (null tray) `NO_HAY_CAMINO
		(if (eq (caar tray) f) (reverse (car tray)) ; Llegamos, devolvemos el camino
			(camino i f grafo (append (
					mapcar (lambda (x) (cons x (car tray))) (diff (vecinos (caar tray) grafo) (car tray)))
					(cdr tray)
				)     ; Una lista con todos los posibles vecinos no visitados para el primer nodo que hay en el actual recorrido
			)
		)
	)
)

;Recibe un nodo inicio, un nodo final y un grafo que los contiene
;Devuelve todos los caminos que encuentra desde el nodo i hasta el nodo f
(defun caminos (i f grafo &optional (tray (list (list i))) results)
	(if (null tray) results ;`NO_HAY_CAMINO
		(if (eq (caar tray) f) (caminos i f grafo (cdr tray) (cons (reverse (car tray)) results)) ; Llegamos, agrego el camino a la lista de caminos results
			(caminos i f grafo (append (
					mapcar (lambda (x) (cons x (car tray))) (diff (vecinos (caar tray) grafo) (car tray)))
					(cdr tray)
				) ; Una lista con todos los posibles vecinos no visitados para el primer nodo que hay en el actual recorrido
				results
			)
		)
	)
)



;(print (caminos `a `k grafo)) ; --> (A B C D N J K)

(defun caminos_minimos (caminos &optional minimos)
	(cond
		((null caminos) minimos)
		((null minimos) (caminos_minimos (cdr caminos) (cons (car caminos) minimos)))
		((< (length (car caminos)) (length (car minimos))) (caminos_minimos (cdr caminos) (list (car caminos))))
		((= (length (car caminos)) (length (car minimos))) (caminos_minimos (cdr caminos) (cons (car caminos) minimos)))
		(T (caminos_minimos (cdr caminos) minimos))
	)
)

;(print (caminos_minimos (caminos `a `k grafo)))



;Recibe una lista con dos calles y el diccionario de los nodos
;Devuelve el nodo que le corresponde a la interseccion de esas calles
(defun nodo (calles diccionario)
	(if (null diccionario) nil
		(if (and (pertenece (nth 0 calles) (cadar diccionario)) (pertenece (nth 1 calles) (cadar diccionario))) (caar diccionario)
			(nodo calles (cdr diccionario))
		)
	)
)

;Opuesto al anterior. Recibe un nodo y el diccionario de los nodos.
;Devuelve la lista de calles que le corresponde a ese nodo
(defun esquina (nodo diccionario)
	(if (null diccionario) nil
		(if (eq nodo (caar diccionario)) (cadar diccionario)
			(esquina nodo (cdr diccionario))
		)
	)
)

;Recibe una lista de nodos y un diccionario
;Devuelve una lista de listas de las calles que le corresponden al nodo
(defun esquinas (camino diccionario) 
	(mapcar (lambda(nodo) (esquina nodo diccionario)) camino)
)

;Recibe dos listas de esquinas
;Devuelve cual es la calle que tienen en comun
(defun calle_union (esq1 esq2)
	(if (or (eq (car esq1) (car esq2)) (eq (car esq1) (cadr esq2))) (car esq1)
		(cadr esq1)
	)
)

;recibe un camino de esquinas (c1 c2)
;devuelve una lista de sublistas con una cuadra y cuadras hay que ir por esa calle 
(defun recorrido (camino &optional (ant nil) (c 0)  (res nil))
	(cond
		((null ant) (recorrido (cdr camino) (calle_union (nth 0 camino) (nth 1 camino))))
		((<= (length camino) 1) (reverse (cons (list ant (+ c 1)) res)))
		((eq (calle_union (nth 0 camino) (nth 1 camino)) ant) (recorrido (cdr camino) ant (+ c 1) res)) ; sigo por la misma calle
		(T (recorrido (cdr camino) (calle_union (nth 0 camino) (nth 1 camino)) 0 (cons (list ant (+ c 1)) res))) ; dobla 
	)
)

(defun armar_hasta_destino(cuadras calle)
	(list `RECCORRER cuadras `CUADRAS `POR calle `HASTA `LLEGAR `A `DESTINO.)
)

(defun armar_hasta_doblar(cuadras calle dobla)
	(list `RECCORRER cuadras `CUADRAS `POR calle `Y `DOBLAR `EN dobla)
)

;Recibe una lista de listas. Cada sublista tiene una calle y la cantidad de cuadras que hay que seguir por esa calle hasta llegar a destino
;Devuelve una lista con el recorrido en listas
(defun instrucciones (recorrido &optional sol)
	(cond 
		((and (eq (length recorrido) 1) (null sol)) (list `YA `ESTAS `EN `EL `DESTINO))
		((eq (length recorrido) 1) 
			(reduce `append (reverse (cons (armar_hasta_destino (cadar recorrido) (caar recorrido)) sol))))
		(T (instrucciones (cdr recorrido) 
			(cons (armar_hasta_doblar (cadar recorrido) (caar recorrido) (car (nth 1 recorrido))) sol)))
	)
)


(defun mostrar_multiples_instrucciones (recorridos &optional (i 1))
	(if (null recorridos) nil
		(cons (append (list 'INSTRUCCIONES 'PARA 'EL 'CAMINO i `:) (instrucciones (car recorridos))) (mostrar_multiples_instrucciones (cdr recorridos) (+ i 1)))
	)
)

(defun mostrar_instrucciones (recorridos)
	(if (eq (length recorridos) 1) (instrucciones (car recorridos))
		(mostrar_multiples_instrucciones recorridos))
)

;Recibe una esquina inicio y una esquina final.
;Imprime por pantalla las instrucciones para llegar de i a f
(defun gps (i f grafo diccionario)
	(mostrar_instrucciones
		(mapcar `recorrido
			(mapcar (lambda (camino) (esquinas camino diccionario)) (caminos_minimos (caminos (nodo i diccionario) (nodo f diccionario) grafo)))
		)
	)
)

(print "-------")
(print (gps `(PaseoColon Independencia) `(Defensa Belgrano) grafo diccionario))

(print "-------")
(print (gps `(Defensa Belgrano) `(Belgrano Defensa) grafo diccionario))

(print "-------")
(print (gps `(Balcarce Chile) `(PaseoColon Belgrano) grafo diccionario))