# scheme-rust

Rust言語によるScheme言語実装.

## コマンド

* repl

```shell
$ scheme-rust-repl
>>((lambda (x) (+ x 1)) 1)
2
```

* interpreter

```shell
$ echo "(car (cons 1 2))" | scheme-rust 
1
```

## 実装済みの機能

### 型

* boolean
* number
* procedure

### 式

#### 変数参照

```scheme
(define x 28)
x ;28
```

#### 手続きの呼び出し

```scheme
(+ 3 4) ;3
((if #f + *) 3 4) ;12
```

#### 手続き

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

#### 条件式

```scheme
(if (> 3 2) 1 2) ;1
(if (> 2 3) 1 2) ;2
(if (> 3 2)
    (- 3 2)
    (+ 3 2)) ;1
```

#### 代入

```scheme
(define x 2)
(+ x 1) ;3
(set! x 4)
(+ x 1) ;5
```

#### 条件式 cond

```scheme
(cond ((> 3 2) 2)
      ((< 3 2) 1)) ;2
(cond ((> 3 3) 3)
      ((< 3 3) 2)
      (else    1)) ;1
```

#### 束縛

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

#### 逐次式

```scheme
(define x 0)
(begin (set! x 5)
    (+ x 1))
    ; #6
```

### プログラム構造

#### 定義

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
(equal? 2 2) ; #t
(equal? (list) (list)) ; '() '() => #t
(equal? (cons 1 2) (cons 1 2)) ; => #t
(equal? #f (list)) ; => #f
(let ((p (lambda (x) x)))
    (equal? p p)); => #t
(equal? car car) ; => #t
```

#### 数

```scheme
(+ 3 4) ; 7
(+ 3)   ; 3
(+)     ; 0
(* 4)   ; 4
(*)     ; 1
```

#### ブーリアン

```scheme
(not #t)       ; #f
(not 3)        ; #f
(not (list 3)) ; #f
(not #f)       ; #t
(not (list))   ; #f
```
