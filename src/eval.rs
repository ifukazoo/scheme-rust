extern crate num;
use crate::env;
use crate::env::*;
use crate::object::*;
use crate::parser::Atom;
use crate::parser::Unit;
use num::Zero;
use std::collections::HashMap;
use std::fmt;

// 評価時エラー
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

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::InvalidApplication(s) => write!(f, "invalid application:[{}].", s),
            Self::InvalidSyntax(s) => write!(f, "invalid syntax:[{}].", s),
            Self::NotImplementedSyntax => write!(f, "not implemented syntax."),
            Self::UnboundVariable(s) => write!(f, "unbound variable:[{}]", s),
            Self::WrongNumberArguments(expected, actual) => write!(
                f,
                "wrong number arguments. expected {}, but {}.",
                expected, actual
            ),
            Self::ZeroDivision => write!(f, "zero division occured."),
        }
    }
}

pub fn eval_program(element: Unit) -> Result<Object, EvalError> {
    let env = new_env(HashMap::new());
    eval(element, &env)
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
        Atom::App(a) => Ok(Object::Subr(a)),
    }
}
fn eval_paren(elements: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if elements.is_empty() {
        Ok(Object::Nil)
    } else {
        // (op operand operand operand ...)
        let op = &elements[0];
        let operand = &elements[1..];
        match op {
            // (+ 1 2), (define ), (lambda ) ,...
            Unit::Bare(Atom::App(o)) => apply(o, operand.to_vec(), env),
            // (myfunc 1 2 3)
            Unit::Bare(Atom::Ident(name)) => match get_value(env, name) {
                None => Err(EvalError::UnboundVariable(name.to_string())),
                Some(Object::Procedure(params, block, closed_env)) => {
                    eval_closure(params, block, closed_env, operand.to_vec(), env)
                }
                Some(Object::Subr(s)) => apply(s, operand.to_vec(), env),
                Some(_) => Err(EvalError::InvalidApplication(format!("{:?}", name))),
            },
            Unit::Bare(atom) => Err(EvalError::InvalidApplication(format!("{:?}", atom))),
            Unit::Paren(units) => {
                // ((lambda (a) a) 0)
                let f = eval_paren(units.clone(), env)?;
                match f {
                    Object::Procedure(params, block, closed_env) => {
                        eval_closure(params, block, closed_env, operand.to_vec(), env)
                    }
                    Object::Subr(s) => apply(s, operand.to_vec(), env),
                    _ => Err(EvalError::InvalidApplication(format!("{:?}", units))),
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
        "begin" => eval_multi(args, env),
        "define" => define(args, env),
        "set" => set(args, env),
        "cons" => cons(args, env),
        "car" => car(args, env),
        "cdr" => cdr(args, env),
        "list" => list(args, env),
        "equal" => equal(args, env),
        "not" => not(args, env),
        "if" => if_exp(args, env),
        "cond" => cond(args, env),
        "let" => let_exp(args, env),
        "leta" => leta_exp(args, env),
        "letrec" => letrec_exp(args, env),
        "lambda" => lambda(args, env),
        "zero" => is_zero(args, env),
        _ => Err(EvalError::InvalidApplication(operation.to_string())),
    }
}

// Number型を要求する引数をNumber型のVectorに変換
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
    let acc = operands.into_iter().sum();
    Ok(Object::Num(acc))
}
fn mul(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(args, env)?;
    let acc = operands.into_iter().product();
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
    // (- 2)
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

    // (/ 2)
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
// 複文の評価
fn eval_multi(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    let mut result = Object::Num(Number::Int(0));
    for a in args.into_iter() {
        result = eval(a, env)?;
    }
    Ok(result)
}
fn set(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidSyntax(
            "set!の引数の数が2でない.".to_string(),
        ));
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
            "defineの引数の数が1でない.".to_string(),
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

    // (define a)
    if args.len() == 1 {
        env::set_value(env, var, Object::Undef);
        // gaucheは aを返す
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
    let obj = args.get(0).unwrap();
    let obj = eval(obj.clone(), env)?;
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
    let obj = args.get(0).unwrap();
    let obj = eval(obj.clone(), env)?;
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
fn equal(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidSyntax("equal?の引数が2以外.".to_string()));
    }
    let left = args.get(0).unwrap();
    let left = eval(left.clone(), env)?;
    let right = args.get(1).unwrap();
    let right = eval(right.clone(), env)?;

    // 左辺と右辺の型が同じ場合のみ比較．違う場合はfalse
    // schemeのequal?とObjectのPartialEqが等しくなるように実装している．
    Ok(Object::Bool(left == right))
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
    // (if cond conseq alt )
    // (if cond conseq)
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
            } else {
                // false
                if args.len() == 2 {
                    Ok(Object::Undef)
                } else {
                    let alt = args.get(2).unwrap();
                    let alt = eval(alt.clone(), env)?;
                    Ok(alt)
                }
            }
        }
        //   cond に bool 式以外の場合は true扱い
        _ => {
            let conseq = args.get(1).unwrap();
            let conseq = eval(conseq.clone(), env)?;
            Ok(conseq)
        }
    }
}
fn cond(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    // (cond (exp exp) (exp exp) (else exp))
    if args.is_empty() {
        return Err(EvalError::InvalidSyntax("cond式の引数が0.".to_string()));
    }

    // 何もマッチするものがなければ undef
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
    if args.is_empty() {
        return Err(EvalError::InvalidSyntax(
            "let式の形式不正. (let)".to_string(),
        ));
    }

    // 変数にバインドするかっこ式部分
    let bind_stmt = args.get(0).unwrap();
    match bind_stmt {
        // (let a ())
        Unit::Bare(_) => Err(EvalError::InvalidSyntax(
            "let式の形式不正. (let no_paren exp)".to_string(),
        )),
        Unit::Paren(units) => {
            let mut params = vec![];
            let mut values = vec![];
            for unit in units {
                match unit {
                    Unit::Bare(_) => {
                        return Err(EvalError::InvalidSyntax(
                            "let式の形式不正. (let (no_paren (b 1)) exp)".to_string(),
                        ))
                    }
                    Unit::Paren(sym_and_value) => {
                        if sym_and_value.len() != 2 {
                            return Err(EvalError::InvalidSyntax(
                                "let式の形式不正. (let ((a param extra) (b param)) exp) "
                                    .to_string(),
                            ));
                        }
                        let sym = sym_and_value.get(0).unwrap();
                        let value = sym_and_value.get(1).unwrap();
                        params.push(sym.clone());
                        values.push(value.clone());
                    }
                }
            }
            // (let (bind_stmt) (...) (...) (...))
            //       0           1     2     3
            let block = if args.len() == 1 {
                None
            } else {
                Some(args[1..].to_vec())
            };
            eval_closure(params, block, env.clone(), values, env)
        }
    }
}
fn leta_exp(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.is_empty() {
        return Err(EvalError::InvalidSyntax(
            "let*式の形式不正. (let*)".to_string(),
        ));
    }

    // 変数にバインドするかっこ式部分
    let bind_stmt = args.get(0).unwrap();
    match bind_stmt {
        // (let* a ())
        Unit::Bare(_) => Err(EvalError::InvalidSyntax(
            "let*式の形式不正. (let* no_paren exp)".to_string(),
        )),
        Unit::Paren(units) => {
            let mut leta_env = env.clone();
            // 束縛部を順に評価する
            for unit in units {
                match unit {
                    Unit::Bare(_) => {
                        return Err(EvalError::InvalidSyntax(
                            "let*式の形式不正. (let* (no_paren (b 1)) exp)".to_string(),
                        ))
                    }
                    Unit::Paren(sym_and_value) => {
                        if sym_and_value.len() != 2 {
                            return Err(EvalError::InvalidSyntax(
                                "let*式の形式不正. (let ((a param extra) (b param)) exp) "
                                    .to_string(),
                            ));
                        }
                        let sym = sym_and_value.get(0).unwrap();
                        let value = sym_and_value.get(1).unwrap();
                        if let Unit::Bare(Atom::Ident(key)) = sym {
                            let inner = new_env(HashMap::new());
                            let value = eval(value.clone(), &leta_env)?;
                            set_value(&inner, key, value);
                            add_outer(&inner, &leta_env);
                            leta_env = inner;
                        }
                    }
                }
            }
            if args.len() == 1 {
                // (let* (...) )
                Ok(Object::Num(Number::Int(0)))
            } else {
                let block = args[1..].to_vec();
                eval_multi(block, &leta_env)
            }
        }
    }
}
fn letrec_exp(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.is_empty() {
        return Err(EvalError::InvalidSyntax(
            "letrec式の形式不正. (letrec)".to_string(),
        ));
    }

    // 変数にバインドするかっこ式部分
    let bind_stmt = args.get(0).unwrap();
    match bind_stmt {
        // (letrec a ())
        Unit::Bare(_) => Err(EvalError::InvalidSyntax(
            "letrec式の形式不正. (letrec no_paren exp)".to_string(),
        )),
        Unit::Paren(units) => {
            let letrec_env = env.clone();
            // 束縛部を順に評価する
            for unit in units {
                match unit {
                    Unit::Bare(_) => {
                        return Err(EvalError::InvalidSyntax(
                            "letrec式の形式不正. (letrec (no_paren (b 1)) exp)".to_string(),
                        ))
                    }
                    Unit::Paren(sym_and_value) => {
                        if sym_and_value.len() != 2 {
                            return Err(EvalError::InvalidSyntax(
                                "letrec式の形式不正. (letrec ((a param extra) (b param)) exp) "
                                    .to_string(),
                            ));
                        }
                        let sym = sym_and_value.get(0).unwrap();
                        let value = sym_and_value.get(1).unwrap();
                        if let Unit::Bare(Atom::Ident(key)) = sym {
                            let value = eval(value.clone(), &letrec_env)?;
                            set_value(&letrec_env, key, value);
                        }
                    }
                }
            }
            if args.len() == 1 {
                // (letrec (...) )
                Ok(Object::Num(Number::Int(0)))
            } else {
                let block = args[1..].to_vec();
                eval_multi(block, &letrec_env)
            }
        }
    }
}

fn lambda(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.is_empty() {
        return Err(EvalError::InvalidSyntax(
            "lambda式の形式不正. (lambda)".to_string(),
        ));
    }

    // (lambda (a b) (...) (...))
    //          0     1     2

    let first = args.get(0).unwrap();
    match first {
        // (lambda a)
        Unit::Bare(_) => Err(EvalError::InvalidSyntax(
            "lambda式の形式不正. (lambda a)".to_string(),
        )),
        Unit::Paren(params) => {
            if args.len() == 1 {
                // (lambda (a b) )
                Ok(Object::Procedure(params.clone(), None, env.clone()))
            } else {
                let block = args[1..].to_vec();
                Ok(Object::Procedure(params.clone(), Some(block), env.clone()))
            }
        }
    }
}

fn is_zero(args: Vec<Unit>, env: &RefEnv) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidSyntax("zero? require 1 arg.".to_string()));
    }
    let value = args.get(0).unwrap();
    let value = eval(value.clone(), env)?;
    match value {
        Object::Num(n) => Ok(Object::Bool(n.is_zero())),
        _ => Err(EvalError::InvalidSyntax(
            "zero? require read number.".to_string(),
        )),
    }
}

fn eval_closure(
    params: Vec<Unit>,
    block: Option<Vec<Unit>>,
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
            // argumentの評価
            let caller_env = new_env(HashMap::new());
            for (param, arg) in params.into_iter().zip(args.into_iter()) {
                if let Unit::Bare(Atom::Ident(key)) = param {
                    let arg = eval(arg.clone(), env)?;
                    set_value(&caller_env, &key, arg);
                }
            }
            add_outer(&caller_env, &closed_env);
            eval_multi(block, &caller_env)
        }
    }
}

// (< 1 2 3 4) とか
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
    let mut prev = operands[0];
    let mut acc = true;
    for operand in operands[1..].iter() {
        acc = cmp(prev, *operand);
        prev = *operand;
    }
    Ok(Object::Bool(acc))
}
