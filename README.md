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
