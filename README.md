# scheme-rust

Rust言語によるScheme風言語実装. Rustの勉強が目的でSchemeの言語仕様に合ってない部分が多々あります．  
A Scheme-like language implementation in Rust. This is an attempt to learn Rust, and there are many parts that don't conform to language specification.

## 型

* boolean
* number
* procedure

## 式

### 変数参照

```scheme
(define x 28)
x ;28
```

### 手続きの呼び出し

```scheme
(+ 3 4) ;3
((if #f + *) 3 4) ;12
```

### 手続き

```scheme
(lambda (x) (+ x x))
((lambda (x) (+ x x)) 4) ;8

(define reverse-subtract
    (lambda (x y) (- y x)))
(reverse-subtract 7 10) ;3

(define add4
    (let ((x 4))
        (lambda (y) (+ x y))))
(add4 6) ;10
```

### 条件式

```scheme
(if (> 3 2) 1 2) ;1
(if (> 2 3) 1 2) ;2
(if (> 3 2)
    (- 3 2)
    (+ 3 2)) ;1
```

### 代入

```scheme
(define x 2)
(+ x 1) ;3
(set! x 4)
(+ x 1) ;5
```

### 条件式 cond

```scheme
(cond ((> 3 2) 2)
      ((< 3 2) 1)) ;2
(cond ((> 3 3) 3)
      ((< 3 3) 2)
      (else    1)) ;1
```

### 束縛

```scheme
(let ((x 2) (y 3))
    (* x y))
    ;6
(let ((x 2) (y 3))
    (let ((x 7)
          (z (+ x y)))
      (* z x)))
    ;35
(let* ((x 2) (y 3))
    (let ((x 7)
          (z (+ x y)))
    (* z x)))
    ;70
(letrec ((even?
            (lambda (n)
                (if (zero? n)
                    #t
                    (odd? (- n 1)))))
         (odd?
            (lambda (n)
                (if (zero? n)
                    #f
                    (even? (- n 1))))))
    (even? 88))
    ; #t
```

### 逐次式

```scheme
(define x 0)
(begin (set! x 5)
    (+ x 1))
    ; #6
```

## プログラム構造

### 定義

```scheme
(define add3
    (lambda (x) (+ x 3)))
(add3 3)
;6
(define first car)
(first (list 1 2))
;1

(let ((x 5))
    (define foo (lambda (y) (bar x y)))
    (define bar (lambda (a b) (+ (* a b) a)))
    (foo (+ x 3)))
;45

(let ((x 5))
    (letrec ((foo (lambda (y) (bar x y)))
             (bar (lambda (a b) (+ (* a b) a))))
        (foo (+ x 3))))
;45
```

### 標準手続き

#### 等価性述語

```scheme
(eqv? 2 2) ; #t
```
