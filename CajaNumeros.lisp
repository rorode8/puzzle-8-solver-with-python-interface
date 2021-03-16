(declaim (optimize (speed 3) (debug 0) (safety 0)))

;Funciones para leer información de la interfaz gráfica
(defun read-array (filename)
  (with-open-file (in filename)
    (loop for num = (read in nil)
          until (null num)
          collect num)))


(defun first-n (lst N tar)
  (if (= N 0) 
    tar
    (append (list (first lst))  (first-n (rest lst) (- N 1) tar) )
  )
)

(defun split-list (lst n)
  (split-list-aux lst n NIL)
)

(defun split-list-aux (lst n tar)
  (cond
    ((null lst) tar)
    (T (append (list (first-n lst n NIL)) (split-list-aux (nthcdr n lst) n tar) ))
  )
)

;Preparar función de costo
(defun preparaCosto (lst)
    (setq costo1 '(0 0 0 0 0 0 0 0 0) costo2 '(0 0 0 0 0 0 0 0 0) costo3 '(0 0 0 0 0 0 0 0 0)
          costo4 '(0 0 0 0 0 0 0 0 0) costo5 '(0 0 0 0 0 0 0 0 0) costo6 '(0 0 0 0 0 0 0 0 0)
          costo7 '(0 0 0 0 0 0 0 0 0) costo8 '(0 0 0 0 0 0 0 0 0) costo9 '(0 0 0 0 0 0 0 0 0))
    (setq uno (car lst) dos (cadr lst) tres (caddr lst) aux (car uno))
    (setf (nth aux costo2) 1 (nth aux costo3) 2 (nth aux costo4) 1 (nth aux costo5) 2 (nth aux costo6) 3 (nth aux costo7) 2 (nth aux costo8) 3 (nth aux costo9) 4)
    (setq aux (cadr uno))
    (setf (nth aux costo1) 1 (nth aux costo3) 1 (nth aux costo4) 2 (nth aux costo5) 1 (nth aux costo6) 2 (nth aux costo7) 3 (nth aux costo8) 2 (nth aux costo9) 3)
    (setq aux (caddr uno))
    (setf (nth aux costo1) 2 (nth aux costo2) 1 (nth aux costo4) 3 (nth aux costo5) 2 (nth aux costo6) 1 (nth aux costo7) 4 (nth aux costo8) 3 (nth aux costo9) 2)
    (setq aux (car dos))
    (setf (nth aux costo1) 1 (nth aux costo2) 2 (nth aux costo3) 3 (nth aux costo5) 1 (nth aux costo6) 2 (nth aux costo7) 1 (nth aux costo8) 2 (nth aux costo9) 3)
    (setq aux (cadr dos))
    (setf (nth aux costo1) 2 (nth aux costo2) 1 (nth aux costo3) 2 (nth aux costo4) 1 (nth aux costo6) 1 (nth aux costo7) 2 (nth aux costo8) 1 (nth aux costo9) 2)
    (setq aux (caddr dos))
    (setf (nth aux costo1) 3 (nth aux costo2) 2 (nth aux costo3) 1 (nth aux costo4) 2 (nth aux costo5) 1 (nth aux costo7) 3 (nth aux costo8) 2 (nth aux costo9) 1)
    (setq aux (car tres))
    (setf (nth aux costo1) 2 (nth aux costo2) 3 (nth aux costo3) 4 (nth aux costo4) 1 (nth aux costo5) 2 (nth aux costo6) 3 (nth aux costo8) 1 (nth aux costo9) 2)
    (setq aux (cadr tres))
    (setf (nth aux costo1) 3 (nth aux costo2) 2 (nth aux costo3) 3 (nth aux costo4) 2 (nth aux costo5) 1 (nth aux costo6) 2 (nth aux costo7) 1 (nth aux costo9) 1)
    (setq aux (caddr tres))
    (setf (nth aux costo1) 4 (nth aux costo2) 3 (nth aux costo3) 2 (nth aux costo4) 3 (nth aux costo5) 2 (nth aux costo6) 1 (nth aux costo7) 2 (nth aux costo8) 1))

;Auxiliar Hamming
(defun compara (lst lst2)
    (cond
        ((null lst) suma)
        ((eq (car lst) (car lst2)) (incf suma) (compara (cdr lst) (cdr lst2)))
        (t (compara (cdr lst) (cdr lst2)))))

;Auxiliar2 Hamming
(defun compara2 (lst meta)
    (if (listp lst) (mapcar #'compara lst meta)))

;Distancia de Hamming
(defun Hamming (lst meta)
    (setq suma 0)
    (compara2 lst meta)
    (- 9 suma))

;Función de costo:
(defun costo (lst)
    (setq uno (car lst) dos (cadr lst) tres (caddr lst))
    (+  (nth (car uno) costo1) 
        (nth (cadr uno) costo2) 
        (nth (caddr uno) costo3) 
        (nth (car dos) costo4) 
        (nth (cadr dos) costo5) 
        (nth (caddr dos) costo6) 
        (nth (car tres) costo7) 
        (nth (cadr tres) costo8) 
        (nth (caddr tres) costo9)))

;Mueve izquierda:
(defun izquierda (lst)
    (setq aux (copy-tree lst))
    (setq uno (car aux) dos (cadr aux) tres (caddr aux))
    (cond
        ((eq (cadr uno) 0) (setf (cadr uno) (car uno) (car uno) 0) aux)
        ((eq (caddr uno) 0) (setf (caddr uno) (cadr uno) (cadr uno) 0) aux)
        ((eq (cadr dos) 0) (setf (cadr dos) (car dos) (car dos) 0) aux)
        ((eq (caddr dos) 0) (setf (caddr dos) (cadr dos) (cadr dos) 0) aux)
        ((eq (cadr tres) 0) (setf (cadr tres) (car tres) (car tres) 0) aux)
        ((eq (caddr tres) 0) (setf (caddr tres) (cadr tres) (cadr tres) 0) aux)
    (t NIL)))
        
;Mueve arriba:
(defun arriba (lst)
    (setq aux (copy-tree lst))
    (setq uno (car aux) dos (cadr aux) tres (caddr aux))
    (cond
        ((eq (car dos) 0) (setf (car dos) (car uno) (car uno) 0) aux)
        ((eq (cadr dos) 0) (setf (cadr dos) (cadr uno) (cadr uno) 0) aux)
        ((eq (caddr dos) 0) (setf (caddr dos) (caddr uno) (caddr uno) 0) aux)
        ((eq (car tres) 0) (setf (car tres) (car dos) (car dos) 0) aux)
        ((eq (cadr tres) 0) (setf (cadr tres) (cadr dos) (cadr dos) 0) aux)
        ((eq (caddr tres) 0) (setf (caddr tres) (caddr dos) (caddr dos) 0) aux)
    (t NIL)))
    
;Mueve derecha:
(defun derecha (lst)
    (setq aux (copy-tree lst))
    (setq uno (car aux) dos (cadr aux) tres (caddr aux))
    (cond
        ((eq (car uno) 0) (setf (car uno) (cadr uno) (cadr uno) 0) aux)
        ((eq (cadr uno) 0) (setf (cadr uno) (caddr uno) (caddr uno) 0) aux)
        ((eq (car dos) 0) (setf (car dos) (cadr dos) (cadr dos) 0) aux)
        ((eq (cadr dos) 0) (setf (cadr dos) (caddr dos) (caddr dos) 0) aux)
        ((eq (car tres) 0) (setf (car tres) (cadr tres) (cadr tres) 0) aux)
        ((eq (cadr tres) 0) (setf (cadr tres) (caddr tres) (caddr tres) 0) aux)
    (t NIL)))
    
;Mueve abajo:
(defun abajo (lst)
    (setq aux (copy-tree lst))
    (setq uno (car aux) dos (cadr aux) tres (caddr aux))
    (cond
        ((eq (car uno) 0) (setf (car uno) (car dos) (car dos) 0) aux)
        ((eq (cadr uno) 0) (setf (cadr uno) (cadr dos) (cadr dos) 0) aux)
        ((eq (caddr uno) 0) (setf (caddr uno) (caddr dos) (caddr dos) 0) aux)
        ((eq (car dos) 0) (setf (car dos) (car tres) (car tres) 0) aux)
        ((eq (cadr dos) 0) (setf (cadr dos) (cadr tres) (cadr tres) 0) aux)
        ((eq (caddr dos) 0) (setf (caddr dos) (caddr tres) (caddr tres) 0) aux)
    (t NIL)))

;Obtener secuencia final (después de haber encontrado nodo final)
(defun obtenSecuencia (lst padre); para el primer ciclo mandar 'padre' como el número del último nodo
    (cond
        ((null (cdr lst)) NIL)
        ((eq (caadar lst) padre) (append (obtenSecuencia (cdr lst) (car (cdadar lst))) (last (cadar lst))))
        (t (obtenSecuencia (cdr lst) padre))))
        
;Contiene lista (revisa si el puzzle q le mandas ya está en cerrado)
(defun contieneLista (puzzle cerrado); puzzle tiene el formato ((a b c)(d e f)(g h i))
    (cond
        ((null cerrado) NIL)
        ((equal puzzle (caddar cerrado)) T)
        (t (contieneLista puzzle (cdr cerrado)))))
        
;Expandir izquierda
(defun expandirIzquierda (nodo)
    (setq puzzle (izquierda (caddr nodo)))
    (cond
        ((null puzzle) NIL)
        ((and (not (null cerrado)) (contieneLista puzzle cerrado)))
        (t (setq man (+ (costo puzzle) (nth 3 (cadr nodo)))) (incf numero) (setq abierto (pq-insert abierto `((,numero ,(caadr nodo) ,(+ (car (cddadr nodo)) 1) ,man 1) ,puzzle)  (max (+ man (Hamming puzzle meta)) (caar nodo)))))))

;Expandir arriba
(defun expandirArriba (nodo)
    (setq puzzle (arriba (caddr nodo)))
    (cond
        ((null puzzle) NIL)
        ((and (not (null cerrado)) (contieneLista puzzle cerrado)))
        (t (setq man (+ (costo puzzle) (nth 3 (cadr nodo)))) (incf numero) (setq abierto (pq-insert abierto `((,numero ,(caadr nodo) ,(+ (car (cddadr nodo)) 1) ,man 2) ,puzzle) (max (+ man (Hamming puzzle meta)) (caar nodo)))))))
        
;Expandir derecha
(defun expandirDerecha (nodo)
    (setq puzzle (derecha (caddr nodo)))
    (cond
        ((null puzzle) NIL)
        ((and (not (null cerrado)) (contieneLista puzzle cerrado)))
        (t (setq man (+ (costo puzzle) (nth 3 (cadr nodo)))) (incf numero) (setq abierto (pq-insert abierto `((,numero ,(caadr nodo) ,(+ (car (cddadr nodo)) 1) ,man 3) ,puzzle) (max (+ man (Hamming puzzle meta)) (caar nodo)))))))
        
;Expandir abajo
(defun expandirAbajo (nodo)
    (setq puzzle (abajo (caddr nodo)))
    (cond
        ((null puzzle) NIL)
        ((and (not (null cerrado)) (contieneLista puzzle cerrado)) NIL)
        (t (setq man (+ (costo puzzle) (nth 3 (cadr nodo)))) (incf numero) (setq abierto (pq-insert abierto `((,numero ,(caadr nodo) ,(+ (car (cddadr nodo)) 1) ,man 4) ,puzzle) (max (+ man (Hamming puzzle meta)) (caar nodo)))))))
    
;Expandir nodo
(defun expandirNodo (nodo)
    (expandirIzquierda nodo)
	(expandirArriba nodo)
    (expandirDerecha nodo)
    (expandirAbajo nodo)
    (push nodo cerrado))
    
;Proceso general
(defun buscaMeta ()
    (setq aux2 (pop abierto)); asignamos a aux2 el nodo más barato
    (cond
        ((and (not (null solucion)) (<= (caar solucion) (caar aux2))) (obtenSecuencia cerrado (caadr solucion)))
        ((equal (caddr aux2) meta) (setq solucion (comparaSolucion solucion aux2)) (expandirNodo aux2) (buscaMeta))
        (t (expandirNodo aux2) (buscaMeta))))
    
;Compara soluciones
(defun comparaSolucion (sol1 sol2)
    (if (or (null sol1) (> (caar sol1) (caar sol2))) sol2 sol1))

;Función final
(defun solvePuzzle (inicio meta)
    (preparaCosto meta)
    (setq cerrado NIL solucion NIL numero 1 abierto `(((,(Hamming inicio meta)) (1 0 0 ,(costo inicio) nil) ,inicio)))
    (buscaMeta))
	
;Priority Queue
(defun pq-insert (Q Elem K)	;;Q = priority queue Elem = nodo , K = cost 
  (if (and (not (null Q)) (<= (car (first (first Q))) K)) ;;si no es nulo y el costo del top  es menor debemos buscar el lugar recursivamente
      (cons (first Q) (pq-insert (rest Q) Elem K)) ;;como es menor, hay que buscar recursivamente el lugar 
    (cons (cons (list K) Elem) Q))) ;;en el caso de que hayamos llegado al lugar que le corresponde, añadimos el en este lugar y nos detenemos
	
(defun write-numeric-list(filename l)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
    (dolist (segment l)
      (format out "~D " segment))
    (format out "~%")))
	
(defun resuelve ()
	(setq inicio (split-list(read-array "output1.txt") 3) meta (split-list(read-array "output2.txt") 3))
	(write-numeric-list "lispoutput.txt" (solvepuzzle inicio meta))
	(exit))
	
(resuelve)