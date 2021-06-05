extern crate num;
use crate::env;
use crate::env::*;
use crate::object::*;
use crate::parser::Atom;
use crate::parser::Element;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// 未実装の構文 (本家のschemeは実装している)
    NotImplementedSyntax,
    /// 演算子でないものが演算子として使用された
    InvalidApplication,
    /// ゼロ除算
    ZeroDivision,
    /// 構文エラー
    InvalidSyntax,
    /// 不明な変数
    UnboundVariable(String),
    General(String),
}

pub fn eval(element: Element, env: &RefEnv) -> Result<Object, EvalError> {
    match element {
        Element::A(a) => eval_atom(a, env),
        Element::V(v) => eval_vector(v, env),
    }
}
pub fn eval_atom(atom: Atom, env: &RefEnv) -> Result<Object, EvalError> {
    match atom {
        Atom::Num(i) => Ok(Object::Num(Number::Int(i))),
        Atom::Ident(i) => match env::get_value(env, &i) {
            Some(obj) => Ok(obj),
            None => Err(EvalError::UnboundVariable(i.to_string())),
        },
        _ => Err(EvalError::NotImplementedSyntax),
    }
}
fn eval_vector(elements: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    if elements.is_empty() {
        Ok(Object::Nil)
    } else {
        let op = &elements[0];
        let operand = &elements[1..];
        match op {
            Element::A(Atom::Ope(o)) => apply(o, operand.to_vec(), env),
            Element::A(Atom::Ident(i)) => {
                if let Some(obj) = env::get_value(env, i) {
                    Ok(obj)
                } else {
                    Err(EvalError::UnboundVariable(i.to_string()))
                }
            }
            _ => Err(EvalError::InvalidApplication),
        }
    }
}
fn apply(operation: &str, args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    match operation {
        "+" => add(args, env),
        "-" => sub(args, env),
        "*" => mul(args, env),
        "/" => div(args, env),
        "<" => lt(args, env),
        ">" => gt(args, env),
        "begin" => begin(args, env),
        "define" => define(args, env),
        "set" => set(args, env),
        "cons" => cons(args, env),
        "car" => car(args, env),
        "cdr" => cdr(args, env),
        _ => Err(EvalError::InvalidApplication),
    }
}
fn to_num_vec(elements: Vec<Element>, env: &RefEnv) -> Result<Vec<Number>, EvalError> {
    let mut v = vec![];
    for n in elements.into_iter() {
        let obj = eval(n, env)?;
        if let Object::Num(n) = obj {
            v.push(n);
        } else {
            return Err(EvalError::General(format!(
                "can't convert to number [{:?}]",
                obj
            )));
        }
    }
    Ok(v)
}
fn add(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(args, env)?;
    let acc = operands.iter().sum();
    Ok(Object::Num(acc))
}
fn mul(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(args, env)?;
    let acc = operands.iter().product();
    Ok(Object::Num(acc))
}
fn sub(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    // 引き算は引数0はNG
    if args.is_empty() {
        return Err(EvalError::General(
            "`-` requires at least one argument.".to_string(),
        ));
    }
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(args, env)?;
    let first = operands[0];
    // lispの引き算は引数1の場合と複数の場合で計算方法が違う．
    if operands.len() == 1 {
        Ok(Object::Num(Number::Int(-1) * first))
    } else {
        let sum = operands[1..].iter().fold(first, |acc, o| acc - *o);
        Ok(Object::Num(sum))
    }
}
fn div(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    // 除算は引数0はNG
    if args.is_empty() {
        return Err(EvalError::General(
            "`/` requires at least one argument.".to_string(),
        ));
    }
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(args, env)?;

    if operands.len() == 1 {
        let divider = operands[0];
        if divider.is_zero() {
            Err(EvalError::ZeroDivision)
        } else {
            Ok(Object::Num(Number::Int(1) / divider))
        }
    } else {
        let mut sum = operands[0];
        for r in operands[1..].iter() {
            if r.is_zero() {
                return Err(EvalError::ZeroDivision);
            }
            sum = sum / *r;
        }
        Ok(Object::Num(sum))
    }
}
fn lt(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    fold_cmp(args, |a, b| a < b, env)
}
fn gt(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    fold_cmp(args, |a, b| a > b, env)
}
fn begin(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    let mut result = Object::Num(Number::Int(0));
    for v in args.into_iter() {
        result = eval(v, env)?;
    }
    Ok(result)
}
fn set(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidSyntax);
    }
    let symbol = match args.get(0).unwrap() {
        // (set! (+ 1 2) ...)
        Element::V(_) => return Err(EvalError::InvalidSyntax),
        Element::A(a) => match a {
            // (set! 1 ...)
            Atom::Num(_) => return Err(EvalError::InvalidSyntax),
            // (set! + ...)
            Atom::Ope(_) => return Err(EvalError::NotImplementedSyntax),
            // (set! a ...)
            Atom::Ident(i) => match env::get_value(env, i) {
                Some(_) => i,
                None => return Err(EvalError::UnboundVariable(i.to_string())),
            },
        },
    };
    let value = args.get(1).unwrap();
    let value = eval(value.clone(), env)?;
    env::set_value(env, symbol, value.clone());
    Ok(value)
}
fn define(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.is_empty() {
        return Err(EvalError::InvalidSyntax);
    }
    let var = match args.get(0).unwrap() {
        // (define (+ 1 2) ...)
        Element::V(_) => return Err(EvalError::NotImplementedSyntax),
        Element::A(a) => match a {
            // (define 1 ...)
            Atom::Num(_) => return Err(EvalError::InvalidSyntax),
            // (define + ...)
            Atom::Ope(_) => return Err(EvalError::NotImplementedSyntax),
            // (define a ...)
            Atom::Ident(i) => i,
        },
    };
    if args.len() == 1 {
        env::set_value(env, var, Object::Undef);
        Ok(Object::Undef)
    } else if args.len() == 2 {
        let value = args.get(1).unwrap();
        let value = eval(value.clone(), env)?;
        env::set_value(env, var, value);
        Ok(Object::Undef)
    } else {
        Err(EvalError::InvalidSyntax)
    }
}
fn cons(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidSyntax);
    }
    let lhs = args.get(0).unwrap();
    let lhs = eval(lhs.clone(), env)?;
    let rhs = args.get(1).unwrap();
    let rhs = eval(rhs.clone(), env)?;
    Ok(cons_pair(lhs, rhs))
}
fn car(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidSyntax);
    }
    let elem = args.get(0).unwrap();
    let obj = eval(elem.clone(), env)?;
    match obj {
        Object::Pair(f, _) => Ok(*f),
        _ => Err(EvalError::InvalidSyntax),
    }
}
fn cdr(args: Vec<Element>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidSyntax);
    }
    let elem = args.get(0).unwrap();
    let obj = eval(elem.clone(), env)?;
    match obj {
        Object::Pair(_, s) => Ok(*s),
        _ => Err(EvalError::InvalidSyntax),
    }
}
fn fold_cmp(
    args: Vec<Element>,
    cmp: fn(Number, Number) -> bool,
    env: &RefEnv,
) -> Result<Object, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::General(
            "application requires at least two argument.".to_string(),
        ));
    }
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(args, env)?;
    let first = operands[0];
    let (acc, _) = operands[1..]
        .iter()
        .fold((true, first), |(_, prev), x| (cmp(prev, *x), *x));
    Ok(Object::Bool(acc))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::env;
    use crate::lexer;
    use crate::object;
    use crate::parser;

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
            ("+", EvalError::NotImplementedSyntax),
            ("(1 1 2)", EvalError::InvalidApplication),
            ("(/ 1 0)", EvalError::ZeroDivision),
            ("(/ 0)", EvalError::ZeroDivision),
            ("(define (+ 1 2))", EvalError::NotImplementedSyntax),
            ("(define (+ 1 2))", EvalError::NotImplementedSyntax),
            ("(define 1)", EvalError::InvalidSyntax),
            ("(define + 2)", EvalError::NotImplementedSyntax),
            ("(define a 1 2)", EvalError::InvalidSyntax),
            ("(car 1)", EvalError::InvalidSyntax),
            ("(car 1 2)", EvalError::InvalidSyntax),
        ];

        let env = env::new_env(HashMap::new());
        for (input, expected) in tests.into_iter() {
            let l = lexer::lex(input).unwrap();
            let p = parser::parse_program(l).unwrap();
            match eval(p, &env) {
                Ok(_) => panic!("expected err. but ok."),
                Err(e) => assert_eq!(expected, e),
            }
        }
    }
    #[test]
    fn test_eval_ng_general() {
        use std::collections::HashMap;

        let tests = vec![
            //
            ("(< 1)", EvalError::General("fake".to_string())),
            ("(> 1)", EvalError::General("fake".to_string())),
        ];

        let env = env::new_env(HashMap::new());
        for (input, _) in tests.into_iter() {
            let l = lexer::lex(input).unwrap();
            let p = parser::parse_program(l).unwrap();
            match eval(p, &env) {
                Ok(_) => panic!("expected err. but ok."),
                Err(EvalError::General(_)) => { /*ok*/ }
                Err(e) => panic!(format!("expected general error. but [{:?}]", e)),
            }
        }
    }
}
