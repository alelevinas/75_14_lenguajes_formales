(defun trasp (M)
	(if (null (car M)) nil
		(cons (mapcar 'car M) (trasp (mapcar 'cdr M)))
	)
)

(defun armar (A B)
	(if (null A) nil
		(append (list (car (last A)) (car (last B))) (armar (butlast A) (butlast B)))
	)
)

(defun juntar (A B)
	(trasp (armar A (trasp B)))
)

(setq n '((a b c d) (e f g h) (i j k l) (m n o p)))

(setq m '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)))

(print n)
(print m)

(print (butlast nil))

;(print (juntar n m))