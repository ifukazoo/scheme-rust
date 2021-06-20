extern crate num;
use crate::env;
use crate::env::*;
use crate::object::*;
use crate::parser::Atom;
use crate::parser::Unit;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// 未実装の構文 (本家のschemeは実装している)
    NotImplementedSyntax,
    /// 演算子でないものが演算子として使用された
    InvalidApplication(String),
    /// ゼロ除算
    ZeroDivision,
    /// 構文エラー
    InvalidSyntax(String),
    /// 不明な変数
    UnboundVariable(String),
    /// 引数異常
    WrongNumberArguments(usize, usize),
}

pub fn eval(element: Unit, env: &RefEnv) -> Result<Object, EvalError> {
    match element {
        Unit::Bare(a) => eval_atom(a, env),
        Unit::Paren(v) => eval_paren(v, env),
    }
}
pub fn eval_atom(atom: Atom, env: &RefEnv) -> Result<Object, EvalError> {
    match atom {
        Atom::Num(i) => Ok(Object::Num(Number::Int(i))),
        Atom::Bool(b) => Ok(Object::Bool(b)),
        Atom::Ident(i) => match env::get_value(env, &i) {
            Some(obj) => Ok(obj),
            None => Err(EvalError::UnboundVariable(i.to_string())),
        },
        _ => Err(EvalError::NotImplementedSyntax),
    }
}
fn eval_paren(elements: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if elements.is_empty() {
        Ok(Object::Nil)
    } else {
        let op = &elements[0];
        let operand = &elements[1..];
        match op {
            // (+ 1 2), (define ), (lambda ) ,...
            Unit::Bare(Atom::App(o)) => apply(o, operand.to_vec(), env),
            // (myfunc 1 2 3)
            Unit::Bare(Atom::Ident(name)) => match get_value(env, name) {
                None => Err(EvalError::UnboundVariable(name.to_string())),
                Some(f) => match f {
                    Object::Closure(params, block, closed_env) => {
                        eval_closure(params, block, closed_env, operand.to_vec(), env)
                    }
                    _ => Err(EvalError::InvalidApplication(format!("{:?}", f))),
                },
            },
            Unit::Bare(a) => Err(EvalError::InvalidApplication(format!("{:?}", a))),
            Unit::Paren(v) => {
                // ((lambda (a) a) 0)
                let f = eval_paren(v.clone(), env)?;
                match f {
                    Object::Closure(params, block, closed_env) => {
                        eval_closure(params, block, closed_env, operand.to_vec(), env)
                    }
                    _ => Err(EvalError::InvalidApplication(format!("{:?}", v))),
                }
            }
        }
    }
}
fn apply(operation: &str, args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
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
        "list" => list(args, env),
        "eq" => eq(args, env),
        "not" => not(args, env),
        "if" => if_exp(args, env),
        "cond" => cond(args, env),
        "let" => let_exp(args, env),
        "lambda" => lambda(args, env),
        _ => Err(EvalError::InvalidApplication(operation.to_string())),
    }
}
fn to_num_vec(elements: Vec<Unit>, env: &RefEnv) -> Result<Vec<Number>, EvalError> {
    let mut v = vec![];
    for n in elements.into_iter() {
        let obj = eval(n, env)?;
        if let Object::Num(n) = obj {
            v.push(n);
        } else {
            return Err(EvalError::InvalidSyntax(format!(
                "四則演算の引数に数字ではないものがある.[{:?}]",
                obj
            )));
        }
    }
    Ok(v)
}
fn add(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(args, env)?;
    let acc = operands.iter().sum();
    Ok(Object::Num(acc))
}
fn mul(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(args, env)?;
    let acc = operands.iter().product();
    Ok(Object::Num(acc))
}
fn sub(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    // 引き算は引数0はNG
    if args.is_empty() {
        return Err(EvalError::InvalidSyntax("引き算の引数が0.".to_string()));
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
fn div(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    // 除算は引数0はNG
    if args.is_empty() {
        return Err(EvalError::InvalidSyntax("割り算の引数が0.".to_string()));
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
fn lt(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    fold_cmp(args, |a, b| a < b, env)
}
fn gt(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    fold_cmp(args, |a, b| a > b, env)
}
fn begin(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    let mut result = Object::Num(Number::Int(0));
    for v in args.into_iter() {
        result = eval(v, env)?;
    }
    Ok(result)
}
fn set(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidSyntax("set!の引数が1でない.".to_string()));
    }
    let symbol = match args.get(0).unwrap() {
        // (set! (+ 1 2) ...)
        Unit::Paren(_) => {
            return Err(EvalError::InvalidSyntax(
                "set!の1st引数がシンボルでない.".to_string(),
            ));
        }
        Unit::Bare(a) => match a {
            // (set! 1 ...)
            Atom::Num(_) | Atom::Bool(_) => {
                return Err(EvalError::InvalidSyntax(format!(
                    "set!の1st引数がシンボルでない.[{:?}]",
                    a
                )))
            }
            // (set! + ...)
            Atom::App(_) => return Err(EvalError::NotImplementedSyntax),
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
fn define(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.is_empty() {
        return Err(EvalError::InvalidSyntax(
            "define required 1 arg.".to_string(),
        ));
    }
    let var = match args.get(0).unwrap() {
        // (define (+ 1 2) ...)
        Unit::Paren(_) => return Err(EvalError::NotImplementedSyntax),
        Unit::Bare(a) => match a {
            // (define 1 ...)
            Atom::Num(_) | Atom::Bool(_) => {
                return Err(EvalError::InvalidSyntax(format!(
                    "set! required symbol arg. but {:?}.",
                    a
                )))
            }

            // (define + ...)
            Atom::App(_) => return Err(EvalError::NotImplementedSyntax),
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
        Err(EvalError::InvalidSyntax("defineの引数が3以上.".to_string()))
    }
}
fn cons(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidSyntax("consの引数が2以外.".to_string()));
    }
    let lhs = args.get(0).unwrap();
    let lhs = eval(lhs.clone(), env)?;
    let rhs = args.get(1).unwrap();
    let rhs = eval(rhs.clone(), env)?;
    Ok(cons_pair(lhs, rhs))
}
fn car(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidSyntax("carの引数が1以外".to_string()));
    }
    let elem = args.get(0).unwrap();
    let obj = eval(elem.clone(), env)?;
    match obj {
        Object::Pair(f, _) => Ok(*f),
        _ => Err(EvalError::InvalidSyntax(
            "carの引数がペアでない.".to_string(),
        )),
    }
}
fn cdr(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidSyntax("cdrの引数が1以外.".to_string()));
    }
    let elem = args.get(0).unwrap();
    let obj = eval(elem.clone(), env)?;
    match obj {
        Object::Pair(_, s) => Ok(*s),
        _ => Err(EvalError::InvalidSyntax(
            "cdrの引数がペアでない.".to_string(),
        )),
    }
}
fn list(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    let mut o = Object::Nil;
    for arg in args.iter().rev() {
        let e = eval(arg.clone(), env)?;
        o = cons_pair(e, o);
    }
    Ok(o)
}
fn eq(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidSyntax("eq?の引数が2以外.".to_string()));
    }
    let left = args.get(0).unwrap();
    let left = eval(left.clone(), env)?;
    let right = args.get(1).unwrap();
    let right = eval(right.clone(), env)?;
    match left {
        Object::Bool(lb) => match right {
            Object::Bool(rb) => Ok(Object::Bool(lb == rb)),
            _ => Ok(Object::Bool(false)),
        },
        Object::Nil => match right {
            Object::Nil => Ok(Object::Bool(true)),
            _ => Ok(Object::Bool(false)),
        },
        Object::Num(ln) => match right {
            Object::Num(rn) => Ok(Object::Bool(ln == rn)),
            _ => Ok(Object::Bool(false)),
        },
        _ => Ok(Object::Bool(false)),
    }
}
fn not(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidSyntax("notの引数が1以外.".to_string()));
    }
    let left = args.get(0).unwrap();
    let left = eval(left.clone(), env)?;
    match left {
        Object::Bool(b) => Ok(Object::Bool(!b)),
        _ => Ok(Object::Bool(false)),
    }
}
fn if_exp(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if !(args.len() == 2 || args.len() == 3) {
        return Err(EvalError::InvalidSyntax(
            "if式の引数が2 or 3 以外.".to_string(),
        ));
    }
    let cond = args.get(0).unwrap();
    let cond = eval(cond.clone(), env)?;
    match cond {
        Object::Bool(b) => {
            if b {
                let conseq = args.get(1).unwrap();
                let conseq = eval(conseq.clone(), env)?;
                Ok(conseq)
            } else if args.len() == 2 {
                Ok(Object::Undef)
            } else {
                let alt = args.get(2).unwrap();
                let alt = eval(alt.clone(), env)?;
                Ok(alt)
            }
        }
        _ => Err(EvalError::InvalidSyntax(
            "if式にbool式以外が指定されている.".to_string(),
        )),
    }
}
fn cond(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    /*
    (cond
        (exp exp)
        (exp exp)
        (else exp)
    )
    */
    if args.is_empty() {
        return Err(EvalError::InvalidSyntax("cond式の引数が0.".to_string()));
    }
    let mut result = Object::Undef;
    for arg in args.into_iter() {
        match arg {
            Unit::Bare(_) => {
                return Err(EvalError::InvalidSyntax(
                    "cond式の節がかっこ形式でない.".to_string(),
                ))
            }
            Unit::Paren(v) => {
                if v.len() != 2 {
                    // (cond (#t 1 3))
                    return Err(EvalError::InvalidSyntax(
                        "cond式の節に引数が2以外のものがある.".to_string(),
                    ));
                }
                let first = v.get(0).unwrap();
                match first {
                    Unit::Bare(Atom::App("else")) => {
                        // (else xxx)は無条件に実行
                        let second = v.get(1).unwrap();
                        result = eval(second.clone(), env)?;
                        break;
                    }
                    _ => {
                        let first = eval(first.clone(), env)?;
                        if let Object::Bool(false) = first {
                            // 何もしない
                            // schemeの condは 条件がfalseだとパス．それ以外はevalとなる
                        } else {
                            let second = v.get(1).unwrap();
                            result = eval(second.clone(), env)?;
                            break;
                        }
                    }
                }
            }
        }
    }
    Ok(result)
}
fn let_exp(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    // (let () () ())
    if args.len() != 2 {
        return Err(EvalError::InvalidSyntax("let式の引数が2以外.".to_string()));
    }
    let let_env = new_env(HashMap::new());
    let set_statement = args.get(0).unwrap();
    let exp = args.get(1).unwrap();
    match set_statement {
        // (let a ())
        Unit::Bare(_) => Err(EvalError::InvalidSyntax(
            "let式の定義部分がかっこ形式でない.".to_string(),
        )),
        Unit::Paren(units) => {
            for unit in units.iter() {
                match unit {
                    // (let (a) ())
                    Unit::Bare(_) => {
                        return Err(EvalError::InvalidSyntax(
                            "let式の定義部分にかっこ形式でないものがある.".to_string(),
                        ))
                    }
                    Unit::Paren(sym_and_value) => {
                        if sym_and_value.len() != 2 {
                            // (let ((a b c)) ())
                            return Err(EvalError::InvalidSyntax(
                                "let式の定義部分に要素数が2以外のものがある.".to_string(),
                            ));
                        }
                        let sym = sym_and_value.get(0).unwrap();
                        let value = sym_and_value.get(1).unwrap();
                        if let Unit::Bare(Atom::Ident(key)) = sym {
                            let value = eval(value.clone(), env)?;
                            set_value(&let_env, &key, value);
                        } else {
                            // (let ((1 0)) ())
                            return Err(EvalError::InvalidSyntax(
                                "let式の定義部分にシンボル以外のものが指定された.".to_string(),
                            ));
                        }
                    }
                }
            }
            add_outer(&let_env, env);
            let obj = eval(exp.clone(), &let_env)?;
            Ok(obj)
        }
    }
}
fn lambda(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    // (lambda () exp)
    if args.len() < 1 {
        return Err(EvalError::InvalidSyntax(
            "lambda式の形式が間違っている.".to_string(),
        ));
    }
    let first = args.get(0).unwrap();
    match first {
        // (lambda a ())
        Unit::Bare(_) => Err(EvalError::InvalidSyntax(
            "lambda式のparams部分がかっこ形式でない.".to_string(),
        )),
        Unit::Paren(params) => {
            if args.len() > 1 {
                let exp = args.get(1).unwrap();
                Ok(Object::Closure(
                    params.clone(),
                    Some(exp.clone()),
                    env.clone(),
                ))
            } else {
                Ok(Object::Closure(params.clone(), None, env.clone()))
            }
        }
    }
}
fn eval_closure(
    params: Vec<Unit>,
    block: Option<Unit>,
    closed_env: RefEnv,
    args: Vec<Unit>,
    env: &RefEnv,
) -> Result<Object, EvalError> {
    if params.len() != args.len() {
        return Err(EvalError::WrongNumberArguments(params.len(), args.len()));
    }
    match block {
        // 評価部のないlambda式
        // ( (lambda ()) )
        None => Ok(Object::Num(Number::Int(0))),
        Some(block) => {
            let call_env = new_env(HashMap::new());
            for (param, arg) in params.into_iter().zip(args.into_iter()) {
                if let Unit::Bare(Atom::Ident(key)) = param {
                    let arg = eval(arg.clone(), env)?;
                    set_value(&call_env, &key, arg);
                }
            }
            add_outer(&call_env, &closed_env);
            eval(block, &call_env)
        }
    }
}
fn fold_cmp(
    args: Vec<Unit>,
    cmp: fn(Number, Number) -> bool,
    env: &RefEnv,
) -> Result<Object, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::InvalidSyntax(
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
            ("(eq? 0 0)", Object::Bool(true)),
            ("(eq? 0 1)", Object::Bool(false)),
            ("(eq? #t #t)", Object::Bool(true)),
            ("(eq? #t #f)", Object::Bool(false)),
            ("(eq? #t 0)", Object::Bool(false)),
            ("(eq? (cons 0 0) (cons 0 0))", Object::Bool(false)),
            ("(eq? (list) (list))", Object::Bool(true)),
            ("(not #f)", Object::Bool(true)),
            ("(not #t)", Object::Bool(false)),
            ("(not 1)", Object::Bool(false)),
            ("(not (cons 1 2))", Object::Bool(false)),
            //
            ("(if #t 1 2)", Object::Num(Int(1))),
            ("(if #f 1 2)", Object::Num(Int(2))),
            ("(if #f 1)", Object::Undef),
            ("(if (eq? 1 1) #f #t)", Object::Bool(false)),
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
            //
            ("(let ((a 1)) (+ a 2))", Object::Num(Int(3))),
            ("(let ((a 1) (b 2)) (+ a b))", Object::Num(Int(3))),
            ("(let () (+ 1 2))", Object::Num(Int(3))),
            //
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
                ((lambda (c d) (+ a b)) 3 4)
                )",
                Object::Num(Int(3)),
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
            ("(1 1 2)", EvalError::InvalidApplication("".to_string())),
            ("(/ 1 0)", EvalError::ZeroDivision),
            ("(/ 0)", EvalError::ZeroDivision),
            ("(< 1)", EvalError::InvalidSyntax("".to_string())),
            ("(> 1)", EvalError::InvalidSyntax("".to_string())),
            ("(define)", EvalError::InvalidSyntax(format!(""))),
            ("(define (+ 1 2))", EvalError::NotImplementedSyntax),
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
            (
                "(let ((1 2)) (+ 1 2))",
                EvalError::InvalidSyntax(format!("")),
            ),
            ("(lambda)", EvalError::InvalidSyntax(format!(""))),
        ];

        let env = env::new_env(HashMap::new());
        for (input, expected) in tests.into_iter() {
            let l = lexer::lex(input).unwrap();
            let p = parser::parse_program(l).unwrap();
            match eval(p, &env) {
                Ok(_) => panic!("expected err. but ok."),
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
}
