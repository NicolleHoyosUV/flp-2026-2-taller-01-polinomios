#lang eopl
;Autores: Adriana Milena Noscue Dagua 2477336, Sebastian Cucalon Astorquiza 2477344
;Santiago Torres Rojas 2380301, Nicolle Camila Hoyos Puin 2380608

(require rackunit rackunit/text-ui)
(require (only-in racket/base exn:fail?))
(require (prefix-in listas: "polinomios-listas.rkt"))
(require (prefix-in procedimientos: "polinomios-procedimientos.rkt"))
(require (prefix-in datatypes: "polinomios-datatypes.rkt"))

;; Coefs-iguales (comprueba el coeficiente de cada exponente de la lista)
(define coefs-iguales
  (lambda (cd pol pares)
    (map (lambda (par) (check-equal? (cd pol (car par)) (cdr par)))
         pares)))

;; Sin-termino (comprueba que el polinomio no tiene termino con ese exponente)
(define sin-termino
  (lambda (cd pol e)
    (check-exn exn:fail? (lambda () (cd pol e)))))

;; Pruebas-interfaz (arma las pruebas de la Parte 4 para una representacion)
(define pruebas-interfaz
  (lambda (nombre pc ins cd el)
    (let ((p (ins (ins (ins (pc 'x) 7 0) -3/2 2) 4 5))   ; 4x^5 - (3/2)x^2 + 7
          (cero (pc 'x)))
      (test-suite nombre

        (test-case "casos funcionales"
          (coefs-iguales cd p '((5 . 4) (2 . -3/2) (0 . 7)))
          (coefs-iguales cd (ins p 1 2) '((5 . 4) (2 . -1/2) (0 . 7)))
          (coefs-iguales cd (ins p 6 3) '((5 . 4) (3 . 6) (2 . -3/2) (0 . 7)))
          (coefs-iguales cd (el p 2) '((5 . 4) (0 . 7)))
          (sin-termino cd (el p 2) 2))

        (test-case "polinomio nulo"
          (sin-termino cd cero 0)
          (check-exn exn:fail? (lambda () (el cero 3)))
          (coefs-iguales cd (ins cero 5 3) '((3 . 5))))

        (test-case "insercion que cancela un termino"
          (coefs-iguales cd (ins p 3/2 2) '((5 . 4) (0 . 7)))
          (sin-termino cd (ins p 3/2 2) 2))

        (test-case "insercion con coeficiente cero"
          (coefs-iguales cd (ins p 0 2) '((5 . 4) (2 . -3/2) (0 . 7)))
          (sin-termino cd (ins p 0 3) 3))

        (test-case "error: exponente negativo"
          (check-exn exn:fail? (lambda () (ins p 5 -1))))

        (test-case "error: exponente no registrado en coeficiente-de"
          (sin-termino cd p 3))

        (test-case "error: exponente no registrado en eliminar-termino"
          (check-exn exn:fail? (lambda () (el p 3))))))))

;; Pruebas de sumar (solo datatypes)
(define pruebas-sumar
  (let ((p (datatypes:insertar-termino
            (datatypes:insertar-termino
             (datatypes:insertar-termino (datatypes:polinomio-cero 'x) 7 0) -3/2 2) 4 5))
        (q (datatypes:insertar-termino
            (datatypes:insertar-termino
             (datatypes:insertar-termino (datatypes:polinomio-cero 'x) 2 1) 1/2 2) -4 5))
        (r (datatypes:insertar-termino
            (datatypes:insertar-termino
             (datatypes:insertar-termino (datatypes:polinomio-cero 'x) -7 0) 3/2 2) -4 5))
        (py (datatypes:insertar-termino (datatypes:polinomio-cero 'y) 5 2)))
    (test-suite "sumar"

      (test-case "suma de polinomios que se cancelan por completo"
        (let ((s (datatypes:sumar p r)))
          (sin-termino datatypes:coeficiente-de s 5)
          (sin-termino datatypes:coeficiente-de s 2)
          (sin-termino datatypes:coeficiente-de s 0)))

      (test-case "suma de polinomios en variables distintas"
        (check-exn exn:fail? (lambda () (datatypes:sumar p py)))))))

;; Ejecucion
(run-tests (pruebas-interfaz "listas"
                             listas:polinomio-cero listas:insertar-termino
                             listas:coeficiente-de listas:eliminar-termino))
(run-tests (pruebas-interfaz "procedimientos"
                             procedimientos:polinomio-cero procedimientos:insertar-termino
                             procedimientos:coeficiente-de procedimientos:eliminar-termino))
(run-tests (pruebas-interfaz "datatypes"
                             datatypes:polinomio-cero datatypes:insertar-termino
                             datatypes:coeficiente-de datatypes:eliminar-termino))
(run-tests pruebas-sumar)