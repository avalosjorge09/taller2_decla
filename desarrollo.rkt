#lang racket

;; Ejercicio 1
(define (contar-positivos lst)
  (length
   (filter (lambda (x) (> x 0))  ; dejamos solo los > 0
           lst)))

;; Prueba
(printf "Ejercicio 1: ~a\n"
        (contar-positivos '(3 -2 7 0 -5 9)))


;; Ejercicio 2
(define (cuadrados-pares lst)
  (map (lambda (x) (* x x))
       (filter even? lst)))

;; Prueba
(printf "Ejercicio 2: ~a\n"
        (cuadrados-pares '(1 2 3 4 5 6 7 8)))


;; Ejercicio 3
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))

;; Prueba
(printf "Ejercicio 3: ~a\n"
        (factorial 5))


;; Ejercicio 4
(define (cubos lst)
  (map (lambda (x) (* x x x))
       lst))

;; Prueba
(printf "Ejercicio 4: ~a\n"
        (cubos '(2 3 4)))


;; Ejercicio 5
(define (sumar-impares lst)
  (foldl +          ; función de acumulación (suma)
         0          ; valor inicial
         (filter odd? lst)))  ; nos quedamos solo con los impares

;; Prueba
(printf "Ejercicio 5: ~a\n"
        (sumar-impares '(1 2 3 4 5 6 7)))


;; Ejercicio 6
(define (contiene-negativos? lst)
  (ormap (lambda (x) (< x 0))  ; ¿es negativo?
         lst))

;; Pruebas
(printf "Ejercicio 6 (lista con negativo): ~a\n"
        (contiene-negativos? '(5 9 -3 2)))  ; #t

(printf "Ejercicio 6 (lista sin negativo): ~a\n"
        (contiene-negativos? '(1 2 3 4)))   ; #f


;; Ejercicio 7
(define (suma-acumulada lst)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc)
                           0
                           (car acc))) ; última suma acumulada
                  acc))
          '()        ; acumulador inicial: lista vacía
          lst)))

;; Prueba
(printf "Ejercicio 7: ~a\n"
        (suma-acumulada '(1 2 3 4)))


;; Ejercicio 8
(define (concatenar-cadenas lst)
  (foldl (lambda (elem acc)
           (string-append acc elem)) ; respeta el orden de la lista
         ""                          ; cadena inicial vacía
         lst))

;; Prueba
(printf "Ejercicio 8: ~a\n"
        (concatenar-cadenas '("Hola" " " "Mundo")))


;; Ejercicio 9
(define (doble-mayores-que-5 lst)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5))
               lst)))

;; Prueba
(printf "Ejercicio 9: ~a\n"
        (doble-mayores-que-5 '(3 6 8 2 10)))


;; Ejercicio 10
(define (invertir-lista lst)
  (foldl (lambda (x acc)
           (cons x acc))   ; vamos metiendo cada elemento al frente
         '()               ; acumulador inicial: lista vacía
         lst))

;; Prueba
(printf "Ejercicio 10: ~a\n"
        (invertir-lista '(1 2 3 4)))


;; Ejercicio 11
(define (cuadrado x)
  (* x x))

(define (aplicar-a-lista f lst)
  (map f lst))

;; Prueba
(printf "Ejercicio 11: ~a\n"
        (aplicar-a-lista cuadrado '(1 2 3 4)))


;; Ejercicio 12
(define (promedio-mayores-que-5 lst)
  (let* ([mayores (filter (lambda (x) (> x 5)) lst)] ; nos quedamos con los > 5
         ;; convertimos a inexact (flotantes) usando map
         [mayores-real (map exact->inexact mayores)]
         ;; sumamos con foldl
         [suma (foldl + 0 mayores-real)])
    (if (null? mayores-real)
        0
        (/ suma (length mayores-real))))) 

;; Prueba
(printf "Ejercicio 12: ~a\n"
        (promedio-mayores-que-5 '(3 8 10 4 9 2 7)))
