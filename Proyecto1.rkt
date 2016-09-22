;-------------------------------------------------------------------

;Lee el archivo txt guardado en la misma direccion del codigo fuente
;Entrada: String con el nombre del archivo
;Salida: Lista con los elementos leidos en el archivo
(define leer-archivo
  (lambda (texto)
    (let ((p (open-input-file texto)))
      (let f ((x (read p)))
        (if (eof-object? x)
            (begin
              (close-input-port p)
              '())
            (cons x (f (read p))))))))

;Funcion que parte los datos leidos en dos listas una de las X's y Y's, y otros para los resultados
(define separarResultadoArchivo
  (lambda(nombreArchivo)
    (separarResultadoArchivo_aux (leer-archivo nombreArchivo) '() '() 0)))

(define separarResultadoArchivo_aux
  (lambda(listaResultado L1 L2 contador)
    (cond((null? (cdr listaResultado)) (list L1 (append L2 (list(car listaResultado)))))
         (else(cond((= contador 0) (separarResultadoArchivo_aux (cdr listaResultado) (append L1 (list (car listaResultado))) L2 (+ contador 1)))
                   (else(cond((= contador 1) (separarResultadoArchivo_aux (cdr listaResultado) (append L1 (list (car listaResultado))) L2 (+ contador 1)))
                             (else(cond((= contador 2)
                                        (separarResultadoArchivo_aux (cdr listaResultado)  L1 (append L2 (list(car listaResultado))) 0)))))))))))

(define arbol
  (lambda(raiz hijIzq hijDer)
    (cond((and (null? hijIzq) (null? hijDer))list raiz)
         (else
          (cond((null? hijDer) (list raiz hijIzq '()))
               (else 
                (cond((null? hijIzq) (list raiz '() hijDer))
                     (else(list raiz hijIzq hijDer)))))))))
;Funcion que determina aleatorioamente un numero entre -10 y 10
(define detNumero
  (lambda ()
    (aux-detNumero (random 11) (random 2))))

(define aux-detNumero
  (lambda(num signo)
    (cond ((= num 0) (detNumero))
    (else
    (cond((= signo 0) (* num -1))
         (else num))))))


(define detOperacion_2
  (lambda ()
    (aux-detOperacion_2  (random) )))

(define aux-detOperacion_2
  (lambda (proba)
   (cond((< proba (/ 0 20)) 'expt) ;0.05
        (else(cond((< proba (/ 1 20)) (elemento (random 5) '(/ bitwise-xor and or expt))) ;0.1
        (else
         (cond ((< proba (/ 1)) (elemento (random 3) '(+ - *))))))))))

(define detOperacion
  (lambda ()
    (aux-detOperacion  (random) )))

(define aux-detOperacion
  (lambda (proba)
   (cond((< proba (/ 0 20)) 'expt) ;0.05
        (else(cond((< proba (/ 1 20)) (elemento (random 3) '(and or expt))) ;0.1
        (else
         (cond ((< proba (/ 1)) (elemento (random 3) '(+ - *)))))))))) ;1

(define detVariable
  (lambda()
    (detVariable_aux(random))))

(define detVariable_aux
  (lambda(probabilidad)
    (cond((< probabilidad (/ 1 3)) 'x)
    (else(cond((< probabilidad (/ 2 3)) 'y)
    (else(cond((> probabilidad (/ 2 3)) (detNumero)))))))))

;Funcion que retorna el elemento en una posicion dada
(define elemento
  (lambda (N L)
    (cond ((null? L) #f)
          (else
           (cond(
                 (= N 0) (car L))
                (else(elemento (- N 1) (cdr L))))))))

;Funcion que genera un individuo aleatoriamente (Funciones aleatorias)
;Entrada:
;Salida: Funcion
;Restricciones:
;(lambda (x) (expt (+ -1 -6) (* x 10)))
(define generarIndividuo
  (lambda ()
    (aux-generarIndividuo(random))))


(define aux-generarIndividuo
  (lambda (op)
          ;(op (op K X) (op Y K))
    (cond((< op (/ 1 10)) ;0.1 / 1 10
          (list(detOperacion)
             (list(detOperacion)
               (list (detOperacion)
                   (list(detOperacion)
                      (arbol (detOperacion_2) (detNumero) 'x)
                      (arbol (detOperacion_2) (detNumero) 'y))
                   (list(detOperacion)
                      (arbol (detOperacion_2) (detNumero) (detVariable))
                      (arbol (detOperacion_2) (detNumero) (detVariable))))
               
               (list(detOperacion)
                    (list(detOperacion)
                         (arbol (detOperacion_2) (detNumero) (detVariable))
                         (arbol (detOperacion_2) (detNumero) (detVariable)))
                    (list(detOperacion)
                         (arbol (detOperacion_2) (detNumero) (detVariable))
                         (arbol (detOperacion_2) (detNumero) (detVariable)))))
             (list(detOperacion)
                  (arbol (detOperacion_2) (detNumero) (detVariable))
                  (arbol (detOperacion_2) (detNumero) (detVariable)))))
         (else(cond((< op (/ 1 5)) ;0.2 / 1 5
               (list (detOperacion) 
                         (list (detOperacion) 
                                   (list (detOperacion)
                                   (arbol(detOperacion_2) (detNumero) 'x)
                                   (arbol(detOperacion_2) (detNumero) 'y)) 
                                   
                                   (list(detOperacion)
                                   (arbol (detOperacion_2) (detNumero)(detVariable) )
                                   (arbol(detOperacion_2) (detNumero) (detVariable))))
                         (list(detOperacion)
                                   (arbol (detOperacion_2) (detNumero)(detVariable) )
                                   (arbol(detOperacion_2) (detNumero) (detVariable)))))
          (else(cond(( < op (/ 4 5)) ;0.8 / 4 5
                (list (detOperacion) (arbol (detOperacion_2) 'x (detNumero)) (arbol (detOperacion_2) 'y (detNumero))))
               
               (else(cond((< op 1) ;1
                    (list (detOperacion) 
                                   (list (detOperacion)
                                   (arbol(detOperacion_2) (detNumero) 'x)
                                   (arbol(detOperacion_2) (detNumero) 'y)) 
                                   
                                   (list(detOperacion)
                                   (arbol (detOperacion_2) (detNumero)(detVariable) )
                                   (arbol(detOperacion_2) (detNumero) (detVariable))))))))))))))
               ;(op(op(op K X) (K Y)) (op (op )))
             

;Generar Poblacion de Individuos                    
(define generarPoblacion
  (lambda(numPob)
    (cond((= numPob 0) '())
         (else
          (cons (generarIndividuo) (generarPoblacion (- numPob 1)))))))    

(define evaluarFuncion
  (lambda(fun x y)
    (with-handlers ([exn:fail? (lambda (exn) (evaluarFuncion (generarIndividuo) x y))])
          (cond ((real? (eval(list(list 'lambda '(x y) fun) x y))) 
                  ;(cond((and (< (eval(list(list 'lambda '(x y) fun) x y)) 9999) (> (eval(list(list 'lambda '(x y) fun) x y)) -9999))
                 (evaluarFuncion_aux (eval(list(list 'lambda '(x y) fun) x y))))
          (else(evaluarFuncion (generarIndividuo) x y))))))
         ;(else(evaluarFuncion (generarIndividuo) x y))))))

(define evaluarFuncion_aux
  (lambda(fun)
   (with-handlers ([exn:fail? (lambda (exn)
                                    fun)])
    (cond((and (real? fun) (< fun 9999))
          fun)
         (else(evaluarFuncion((generarIndividuo) x y)))))))

;Caclculo para el valor de un individuo resta el z con el resultado de evaluar la funcion del individuo
;y se suma a un acumulado, el cual es retornado
;Entrada: nombre del archivo y funcion a evaluar
;Salida: valor de la funcion evaluada con los valores de X y Y respecyivos y con Z
;Restricciones: Si el numero no es real entonces
(define calcularValorIndividuo
  (lambda(nombreArch fun)
    (calcularValorIndividuo_intermediario (separarResultadoArchivo nombreArch) fun nombreArch)))

(define calcularValorIndividuo_intermediario
  (lambda (listaResultado fun nombreArch)
    (calcularValorIndividuo_aux (car listaResultado) (car (cdr listaResultado)) fun  0 nombreArch 0)))

(define calcularValorIndividuo_aux
  (lambda (listaXY listaZ fun acumulado nombreArch contador)
    (cond((equal? '() (cdr(cdr listaXY))) 
          (cond((real? (evaluarFuncion fun (car listaXY) (car(cdr listaXY))))
              (+ acumulado (abs(- (evaluarFuncion fun (car listaXY) (car(cdr listaXY))) (car listaZ)))))
                 (else 999)))          
         (else(cond((< (evaluarFuncion fun (car listaXY) (car(cdr listaXY))) 999)
          ;(cond((real? (evaluarFuncion fun (car listaXY) (car(cdr listaXY))))
          (calcularValorIndividuo_aux (cdr(cdr listaXY)) (cdr listaZ) fun 
                 (+ acumulado (abs(- (evaluarFuncion fun (car listaXY) (car(cdr listaXY))) (car listaZ))))  nombreArch (+ contador 1)))
                   (else 999))))))
                   ;(else 99999))))))
 
(define calcularValorIndividuo_lista
  (lambda (nombreArchivo poblacion)
     (calcularValorIndividuo_lista_aux nombreArchivo poblacion '())))

(define calcularValorIndividuo_lista_aux
  (lambda (nombreArchivo poblacion listaNueva)
    (cond((null? (cdr poblacion)) (append (list(calcularValorIndividuo nombreArchivo (car poblacion))) listaNueva))
       (else
          (calcularValorIndividuo_lista_aux 
           nombreArchivo (cdr poblacion) (append (list(calcularValorIndividuo nombreArchivo (car poblacion))) listaNueva))))))

;Funcion que encuentra el numero menor de una lista
(define menorLista
  (lambda (L1)
    (menorLista_aux (cdr L1) (car L1) 0 0)))

(define menorLista_aux
  (lambda (L1 menor cont pos)
    
    (cond((null? (cdr L1)) 
          (cond ((> menor (car L1))(+ cont 1))
                (else pos)))
         (else
          (cond ((> menor (car L1)) (menorLista_aux (cdr L1) (car L1) (+ cont 1) (+ cont 1)))
                (else
                 (menorLista_aux (cdr L1) menor  (+ cont 1) pos)))))))

(define funcionObjetivo
  (lambda (funPob LX LY)
    (elemento (menorLista(calcPoblacion funPob LX LY)) funPob)))

;despues de retornar 
(define mejorElementoPoblacion
  (lambda (nombreArchivo poblacion)
    ;mejorValorIndividuo restoPoblacion 
    (mejorElementoPoblacion_aux (calcularValorIndividuo nombreArchivo (car poblacion)) (car poblacion) (cdr poblacion) nombreArchivo))) ;(calcularValorIndividuo_intermediario (separarResultadoArchivo nombreArchivo) (car poblacion) nombreArchivo)
                                

(define mejorElementoPoblacion_aux
  (lambda (mejorIndividuo mejorFuncion restoPoblacion nombreArchivo)
    (cond((null?  restoPoblacion) mejorFuncion)
         (else(cond((> mejorIndividuo
                     (calcularValorIndividuo nombreArchivo  (car restoPoblacion)))
            (mejorElementoPoblacion_aux 
             (calcularValorIndividuo nombreArchivo  (car restoPoblacion)) (car restoPoblacion) (cdr restoPoblacion) nombreArchivo))
              (else(mejorElementoPoblacion_aux mejorIndividuo mejorFuncion (cdr restoPoblacion) nombreArchivo)))))))

;Para calcular el mejor individuo de una poblacion se recomienda que la poblacion sea 200 o menor

;Función que hace el cruce de dos individuos de la población

(define cruce
  (lambda (individuo1 individuo2)
    (if (null? individuo1)
        '()
    (if (null? individuo2)
        '()
         (list (car individuo1)(cadr individuo1)(cadr (cdr individuo2)))))))
;Función que realiza el proceso completo de cruces y mutaciones

(define cruce_poblacion
  (lambda (poblacion)
    (cruce_poblacion_aux poblacion (car poblacion))
  )
)

(define cruce_poblacion_aux
  (lambda (poblacion primer_elemento)
    (cond ((null? (cdr poblacion)) (append(list(cruce (car poblacion) primer_elemento))))
          (else
           (append (list(cruce (car poblacion) (cadr poblacion))) (cruce_poblacion_aux (cdr poblacion) primer_elemento))))))


;--------------------------------------------------------------------------------------------------------------------

;Función reemplaza en índice, un valor de una lista

(define (reemplazar-en-indice individuo idx val)
  (if (null? individuo)
    individuo
    (cons
      (if (zero? idx)
        val
        (car individuo))
      (reemplazar-en-indice (cdr individuo) (- idx 1) val))))

;Función que muta con una probabilidad de un 3% cada individuo de la población

(define mutacion
  (lambda (individuo1)
    (mutacion_aux individuo1 (random 99))))

(define mutacion_aux
  (lambda (individuo1 random)
    (cond ((= 0 random) (reemplazar-en-indice individuo1 0 (detOperacion_2)))
          (else
           (cond ((= 1 random) (reemplazar-en-indice individuo1 1 (reemplazar-en-indice (cadr individuo1) 0 (detOperacion_2))))
                 (else
                  (cond ((= 2 random) (reemplazar-en-indice individuo1 2 (reemplazar-en-indice (cadr (cdr individuo1)) 0 (detOperacion_2))))
                        (else
                         individuo1))))))))

(define mutacion_poblacion
  (lambda (poblacion)
    (cond ((null? (cdr poblacion)) (append (list (mutacion (car poblacion)))))
          (else
           (append (list (mutacion (car poblacion))) (mutacion_poblacion (cdr poblacion)))))))

(define cruce_y_mutacion
  (lambda (individuo1 individuo2)
    (cruce(mutacion individuo1) (mutacion individuo2))))

(define cruce_y_mutacion_poblacion
  (lambda (poblacion)
    (cruce_y_mutacion_poblacion_aux poblacion (car poblacion))))

(define cruce_y_mutacion_poblacion_aux 
  (lambda(poblacion primer_elemento)
    (cond((null? (cdr poblacion)) 
      (append(list(cruce_y_mutacion (car poblacion) primer_elemento))))
          (else
           (append(list(cruce_y_mutacion (car poblacion) (car (cdr poblacion)))) (cruce_y_mutacion_poblacion_aux (cdr poblacion) primer_elemento))))))

(define genetica
  (lambda (nombreArchivo numeroGeneraciones numeroPoblacion)
  (generacion_aux nombreArchivo numeroGeneraciones (generarPoblacion numeroPoblacion) 1)))

(define generacion_aux
  (lambda (nombreArchivo numeroGeneraciones poblacion contador )
    (cond((= numeroGeneraciones contador) 
          ;(or (= numeroGeneraciones contador) (= (menorLista(calcularValorIndividuo_lista nombreArchivo poblacion)) 0))  
              (mejorElementoPoblacion nombreArchivo poblacion))
         (else
          (generacion_aux nombreArchivo numeroGeneraciones 
                          (cruce_y_mutacion_poblacion poblacion) (+ contador 1) )))))