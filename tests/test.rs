use scheme_rust::env;
use scheme_rust::eval::{eval, EvalError};
use scheme_rust::lexer;
use scheme_rust::object;
use scheme_rust::object::build_list;
use scheme_rust::parser;

#[test]
fn test_eval() {
    use num::rational::Ratio;
    use object::cons_pair;
    use object::Number::Int;
    use object::Number::Rat;
    use object::Object;
    use std::collections::HashMap;

    let tests = vec![
        //
        ("1", Object::Num(Int(1))),
        ("()", Object::Nil),
        ("(+ 1 2 )", Object::Num(Int(3))),
        ("(+ 1 2 3 4 5 6 7 8 9 10)", Object::Num(Int(55))),
        ("(+ 1)", Object::Num(Int(1))),
        ("(+)", Object::Num(Int(0))),
        //
        ("-1", Object::Num(Int(-1))),
        ("(+ +1 2)", Object::Num(Int(3))),
        ("(+ 0 -1)", Object::Num(Int(-1))),
        ("(+ +1 +2 +3 +4 +5 +6 +7 +8 +9 +10)", Object::Num(Int(55))),
        ("(+ -1 -2 -3 -4 -5 -6 -7 -8 -9 -10)", Object::Num(Int(-55))),
        //
        ("(- 1)", Object::Num(Int(-1))),
        ("(- 0 1 )", Object::Num(Int(-1))),
        ("(- 1 2)", Object::Num(Int(-1))),
        ("(- 1 2 3 4 5 6 7 8 9 10)", Object::Num(Int(-53))),
        ("(- +2 +2 +2)", Object::Num(Int(-2))),
        //
        ("(+ (+ 1 2) 3)", Object::Num(Int(6))),
        ("(- (+ (+ 1 2) 3) 5)", Object::Num(Int(1))),
        //
        ("(*)", Object::Num(Int(1))),
        ("(* 1)", Object::Num(Int(1))),
        ("(* 0 1 )", Object::Num(Int(0))),
        ("(* 1 2)", Object::Num(Int(2))),
        ("(* 2 2)", Object::Num(Int(4))),
        ("(* 2 -2)", Object::Num(Int(-4))),
        ("(* -2 2)", Object::Num(Int(-4))),
        ("(* -2 -2)", Object::Num(Int(4))),
        ("(* 2 3 4)", Object::Num(Int(24))),
        ("(+ (* 2 3) 5)", Object::Num(Int(11))),
        //
        ("(/ 1 2 )", Object::Num(Rat(Ratio::new(1, 2)))),
        ("(/ 4 2)", Object::Num(Int(2))),
        ("(/ 1)", Object::Num(Int(1))),
        ("(/ 2)", Object::Num(Rat(Ratio::new(1, 2)))),
        ("(/ 1 2 3 4 5)", Object::Num(Rat(Ratio::new(1, 120)))),
        //
        ("(< 1 2 )", Object::Bool(true)),
        ("(< 2 1 )", Object::Bool(false)),
        ("(< 1 1 )", Object::Bool(false)),
        ("(< 1 (+ 1 1))", Object::Bool(true)),
        ("(< (- 2 1) (+ 1 1))", Object::Bool(true)),
        ("(< 1 2 3 4 5)", Object::Bool(true)),
        ("(< 1 2 3 4 1)", Object::Bool(false)),
        //
        ("(> 2 1)", Object::Bool(true)),
        ("(> 1 2)", Object::Bool(false)),
        ("(> 1 1 )", Object::Bool(false)),
        ("(> (+ 1 1) 1)", Object::Bool(true)),
        ("(> (+ 1 1) (- 2 1))", Object::Bool(true)),
        ("(> 1 2 3 4 5)", Object::Bool(false)),
        ("(> 1 2 3 4 1)", Object::Bool(true)),
        //
        ("(begin (+ 2 1))", Object::Num(Int(3))),
        ("(begin (+ 2 1) (+ 2 3))", Object::Num(Int(5))),
        ("(begin (+ 2 1) (+ 2 3) ())", Object::Nil),
        //
        ("(begin (define a) a)", Object::Undef),
        ("(begin (define a 1) a)", Object::Num(Int(1))),
        ("(begin (define a (+ 1 2)) a)", Object::Num(Int(3))),
        ("(define a)", Object::Undef),
        //
        ("(begin (define a) (set! a 1) a)", Object::Num(Int(1))),
        ("(begin (define a) (set! a (+ 1 2)) a)", Object::Num(Int(3))),
        (
            "(begin
                  (define x 2)
                  (+ x 1)
                  (set! x 4)
                  (+ x 1)
                  )",
            Object::Num(Int(5)),
        ),
        //
        (
            "(cons 1 2)",
            cons_pair(Object::Num(Int(1)), Object::Num(Int(2))),
        ),
        //
        ("(car (cons 1 2))", Object::Num(Int(1))),
        ("(cdr (cons 1 2))", Object::Num(Int(2))),
        ("(car (cons 2 ()))", Object::Num(Int(2))),
        ("(cdr (cons 2 ()))", Object::Nil),
        ("(car (cons (+ 3 4) 2))", Object::Num(Int(7))),
        ("(cdr (cons (+ 3 4) 2))", Object::Num(Int(2))),
        ("(car (cons 1 (cons 2 (cons 3 ()))))", Object::Num(Int(1))),
        (
            "(cdr (cons 1 (cons 2 (cons 3 ()))))",
            cons_pair(
                Object::Num(Int(2)),
                cons_pair(Object::Num(Int(3)), Object::Nil),
            ),
        ),
        //
        ("(list 1)", build_list(vec![Object::Num(Int(1))])),
        (
            "(list 1 2)",
            build_list(vec![Object::Num(Int(1)), Object::Num(Int(2))]),
        ),
        (
            "(list (+ 1 2) (define a) (> 2 1))",
            build_list(vec![Object::Num(Int(3)), Object::Undef, Object::Bool(true)]),
        ),
        ("(list)", Object::Nil),
        //
        (
            "(list #t #f)",
            build_list(vec![Object::Bool(true), Object::Bool(false)]),
        ),
        //
        ("(not #f)", Object::Bool(true)),
        ("(not #t)", Object::Bool(false)),
        ("(not 1)", Object::Bool(false)),
        ("(not (cons 1 2))", Object::Bool(false)),
        // 条件式
        ("(if (> 3 2) 1 2)", Object::Num(Int(1))),
        ("(if (> 2 3) 1 2)", Object::Num(Int(2))),
        ("(if #f 1 2)", Object::Num(Int(2))),
        ("(if #f 1)", Object::Undef),
        ("(if (eqv? 1 1) #f #t)", Object::Bool(false)),
        ("(if (lambda ()) 1 2)", Object::Num(Int(1))),
        (
            "(if (> 3 2)
                     (- 3 2)
                     (+ 3 2))",
            Object::Num(Int(1)),
        ),
        //
        ("(cond (#f 1) (#f 2) (#t 3) (else 4))", Object::Num(Int(3))),
        ("(cond (#t 1) (#f 2) (#t 3) (else 4))", Object::Num(Int(1))),
        ("(cond (#t 1))", Object::Num(Int(1))),
        ("(cond (#f 1) (#f 2))", Object::Undef),
        ("(cond (else 1))", Object::Num(Int(1))),
        (
            "(cond ((define a) 1) (#f 2) (#t 3) (else 4))",
            Object::Num(Int(1)),
        ),
        (
            "(cond ((+ 1 1) 1) (#f 2) (#t 3) (else 4))",
            Object::Num(Int(1)),
        ),
        (
            "(cond ((> 3 2) 2)
                       ((< 3 2) 1))",
            Object::Num(Int(2)),
        ),
        (
            "(cond ((> 3 3) 3)
                       ((< 3 3) 2)
                       (else    1))",
            Object::Num(Int(1)),
        ),
        //
        ("(let ((a 1)) (+ a 2))", Object::Num(Int(3))),
        ("(let ((a 1) (b 2)) (+ a b))", Object::Num(Int(3))),
        ("(let ((1 2)) (+ 3 4))", Object::Num(Int(7))),
        ("(let () (+ 1 2))", Object::Num(Int(3))),
        (
            "(let ((x 2) (y 3))
                   (* x y))",
            Object::Num(Int(6)),
        ),
        (
            "
            (let ((x 2) (y 3))
              (let ((x 7)
                    (z (+ x y)))
                (* z x)))",
            Object::Num(Int(35)),
        ),
        //
        (
            "
            (let ((x 2) (y 3))
              (let* ((x 7)
                    (z (+ x y)))
                (* z x)))",
            Object::Num(Int(70)),
        ),
        (
            "(let ((mul (lambda (a b) (* a b))))
                (mul 2 3))",
            Object::Num(Int(6)),
        ),
        //
        (
            "
            (let ((x 2) (y 3))
              (let* ((x 7)
                    (z (+ x y)))
                (* z x)))",
            Object::Num(Int(70)),
        ),
        (
            "
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
                (even? 88))",
            Object::Bool(true),
        ),
        ("((lambda () 1))", Object::Num(Int(1))),
        ("((lambda (p) p) 2)", Object::Num(Int(2))),
        ("((lambda (a b) (+ a b)) 1 2)", Object::Num(Int(3))),
        (
            "(
                begin
                (define a 1)
                (define b 2)
                ((lambda (a b) (+ a b)) 3 4)
                )",
            Object::Num(Int(7)),
        ),
        (
            "(
                begin
                (define a 1)
                (define b 2)
                ((lambda (c d) (+ a b c d)) 3 4)
                )",
            Object::Num(Int(10)),
        ),
        ("((lambda ()))", Object::Num(Int(0))),
        ("((lambda (a b c)) 1 2 3)", Object::Num(Int(0))),
        //
        (
            "(
                begin
                (define add (lambda (a b) (+ a b)))
                (add 1 2)
                )",
            Object::Num(Int(3)),
        ),
        (
            "(
                begin
                (define add)
                (set! add (lambda (a b) (+ a b)))
                (add 1 2)
                )",
            Object::Num(Int(3)),
        ),
        (
            "(
                begin
                (define abs)
                (set! abs (lambda (a) (if (< a 0) (* -1 a) a)))
                (abs 2)
                )",
            Object::Num(Int(2)),
        ),
        //
        ("((if #f + *) 3 4)", Object::Num(Int(12))),
        (
            "(
                begin
                (define a +)
                (a 1 2)
                )",
            Object::Num(Int(3)),
        ),
        //
        ("(zero? 0)", Object::Bool(true)),
        ("(zero? 1)", Object::Bool(false)),
        ("(zero? (* 1 0))", Object::Bool(true)),
        ("(zero? (/ 3 2))", Object::Bool(false)),
        ("(zero? ((lambda () 0)))", Object::Bool(true)),
        //
        (
            "(begin
            (define add3
                (lambda (x) (+ x 3)))
                (add3 3)
            )",
            Object::Num(Int(6)),
        ),
        (
            "(begin
            (define first car)
            (first (list 1 2))
            )",
            Object::Num(Int(1)),
        ),
        //
        (
            "((lambda ()
            (+ 1 2)
            (+ 3 4)))",
            Object::Num(Int(7)),
        ),
        (
            "((lambda ()
            (define first car)
            (first (list 1 2))
            ))",
            Object::Num(Int(1)),
        ),
        (
            "
        (let ((x 5))
                (define foo (lambda (y) (bar x y)))
                (define bar (lambda (a b) (+ (* a b) a)))
            (foo (+ x 3)))
        ",
            Object::Num(Int(45)),
        ),
        (
            "
        (let* ((x 5))
                (define foo (lambda (y) (bar x y)))
                (define bar (lambda (a b) (+ (* a b) a)))
            (foo (+ x 3)))
        ",
            Object::Num(Int(45)),
        ),
        (
            "
        (letrec ((x 5))
                (define foo (lambda (y) (bar x y)))
                (define bar (lambda (a b) (+ (* a b) a)))
            (foo (+ x 3)))
        ",
            Object::Num(Int(45)),
        ),
        (
            "
        (let ((x 5))
            (letrec ((foo (lambda (y) (bar x y)))
                     (bar (lambda (a b) (+ (* a b) a))))
            (foo (+ x 3))))
        ",
            Object::Num(Int(45)),
        ),
        // 標準手続き
        ("(eqv? 2 2)", Object::Bool(true)),
        // ("(eq? (list) (list))", Object::Bool(true)),
        // ("(eq? car car)", Object::Bool(true)),
        // ("(eq? (lambda (x) x) (lambda (x) x))", Object::Bool(true)),
        // ("(let ((p (lambda (x) x)))(eq? p p))", Object::Bool(true)),
        // ("(eq? 0 0)", Object::Bool(true)),
        // ("(eq? 0 1)", Object::Bool(false)),
        // ("(eq? #t #t)", Object::Bool(true)),
        // ("(eq? #t #f)", Object::Bool(false)),
        // ("(eq? #t 0)", Object::Bool(false)),
        // ("(eq? (cons 0 0) (cons 0 0))", Object::Bool(false)),
    ];
    let env = env::new_env(HashMap::new());
    for (input, expected) in tests.into_iter() {
        let object = eval(
            parser::parse_program(lexer::lex(input).unwrap()).unwrap(),
            &env,
        )
        .unwrap();
        assert_eq!(expected, object)
    }
}
#[test]
fn test_eval_ng() {
    use std::collections::HashMap;

    let tests = vec![
        //
        ("(1 1 2)", EvalError::InvalidApplication("".to_string())),
        ("(/ 1 0)", EvalError::ZeroDivision),
        ("(/ 0)", EvalError::ZeroDivision),
        ("(< 1)", EvalError::InvalidSyntax("".to_string())),
        ("(> 1)", EvalError::InvalidSyntax("".to_string())),
        ("(define)", EvalError::InvalidSyntax(format!(""))),
        ("(define (+ 1 2))", EvalError::NotImplementedSyntax),
        ("(define 1)", EvalError::InvalidSyntax(format!(""))),
        ("(define + 2)", EvalError::NotImplementedSyntax),
        ("(define a 1 2)", EvalError::InvalidSyntax(format!(""))),
        ("(car 1)", EvalError::InvalidSyntax(format!(""))),
        ("(car 1 2)", EvalError::InvalidSyntax(format!(""))),
        ("(not)", EvalError::InvalidSyntax(format!(""))),
        ("(not 1 2)", EvalError::InvalidSyntax(format!(""))),
        ("(not 1 2 3)", EvalError::InvalidSyntax(format!(""))),
        ("(if)", EvalError::InvalidSyntax(format!(""))),
        ("(if #t)", EvalError::InvalidSyntax(format!(""))),
        ("(if #t 1 2 3)", EvalError::InvalidSyntax(format!(""))),
        ("(let  a (+ 1 2))", EvalError::InvalidSyntax(format!(""))),
        ("(let (a) (+ 1 2))", EvalError::InvalidSyntax(format!(""))),
        ("(let (a 1) (+ 1 2))", EvalError::InvalidSyntax(format!(""))),
        (
            "(let ((a 1 2)) (+ 1 2))",
            EvalError::InvalidSyntax(format!("")),
        ),
        ("(lambda)", EvalError::InvalidSyntax(format!(""))),
        ("(zero? #t)", EvalError::InvalidSyntax(format!(""))),
        ("(zero? (list 1 2))", EvalError::InvalidSyntax(format!(""))),
    ];

    let env = env::new_env(HashMap::new());
    for (input, expected) in tests.into_iter() {
        let l = lexer::lex(input).unwrap();
        let p = parser::parse_program(l).unwrap();
        match eval(p, &env) {
            Ok(_) => panic!("expected err. but ok. {:?}", input),
            Err(e) => match expected {
                EvalError::ZeroDivision | EvalError::NotImplementedSyntax => {
                    assert_eq!(expected, e)
                }
                EvalError::InvalidSyntax(_) => {
                    if let EvalError::InvalidSyntax(_) = e {
                    } else {
                        panic!("expected {:?}. but {:?}.", expected, e);
                    }
                }
                EvalError::InvalidApplication(_) => {
                    if let EvalError::InvalidApplication(_) = e {
                    } else {
                        panic!("expected {:?}. but {:?}.", expected, e);
                    }
                }
                EvalError::UnboundVariable(_) => {
                    if let EvalError::UnboundVariable(_) = e {
                    } else {
                        panic!("expected {:?}. but {:?}.", expected, e);
                    }
                }
                EvalError::WrongNumberArguments(_, _) => {
                    if let EvalError::UnboundVariable(_) = e {
                    } else {
                        panic!("expected {:?}. but {:?}.", expected, e);
                    }
                }
            },
        }
    }
}
