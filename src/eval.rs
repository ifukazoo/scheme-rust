extern crate num;
use crate::object::Number;
use crate::object::Object;
use crate::parser::Atom;
use crate::parser::Element;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    NotImplementedSyntax,
    InvalidApplication,
    ZeroDivision,
    General(String),
}

pub fn eval(element: Element) -> Result<Object, EvalError> {
    match element {
        Element::A(a) => eval_atom(a),
        Element::V(v) => eval_vector(v),
    }
}
pub fn eval_atom(atom: Atom) -> Result<Object, EvalError> {
    match atom {
        Atom::Num(i) => Ok(Object::Num(Number::Int(i))),
        _ => Err(EvalError::NotImplementedSyntax),
    }
}
fn eval_vector(elements: Vec<Element>) -> Result<Object, EvalError> {
    if elements.is_empty() {
        Ok(Object::Nil)
    } else {
        let op = &elements[0];
        let operand = &elements[1..];
        match op {
            Element::A(Atom::Ope(o)) => apply(o, operand.to_vec()),
            _ => Err(EvalError::InvalidApplication),
        }
    }
}
fn apply(operation: &str, elements: Vec<Element>) -> Result<Object, EvalError> {
    match operation {
        "+" => add(elements),
        "-" => sub(elements),
        "*" => mul(elements),
        "/" => div(elements),
        "<" => lt(elements),
        ">" => gt(elements),
        _ => Err(EvalError::InvalidApplication),
    }
}
fn to_num_vec(elements: Vec<Element>) -> Result<Vec<Number>, EvalError> {
    let mut v = vec![];
    for n in elements.into_iter() {
        let obj = eval(n)?;
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
fn add(elements: Vec<Element>) -> Result<Object, EvalError> {
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(elements)?;
    let acc = operands.iter().sum();
    Ok(Object::Num(acc))
}
fn mul(elements: Vec<Element>) -> Result<Object, EvalError> {
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(elements)?;
    let acc = operands.iter().product();
    Ok(Object::Num(acc))
}
fn sub(elements: Vec<Element>) -> Result<Object, EvalError> {
    // 引き算は引数0はNG
    if elements.is_empty() {
        return Err(EvalError::General(
            "`-` requires at least one argument.".to_string(),
        ));
    }
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(elements)?;
    let first = operands[0];
    // lispの引き算は引数1の場合と複数の場合で計算方法が違う．
    if operands.len() == 1 {
        Ok(Object::Num(Number::Int(-1) * first))
    } else {
        let sum = operands[1..].iter().fold(first, |acc, o| acc - *o);
        Ok(Object::Num(sum))
    }
}
fn div(elements: Vec<Element>) -> Result<Object, EvalError> {
    // 除算は引数0はNG
    if elements.is_empty() {
        return Err(EvalError::General(
            "`/` requires at least one argument.".to_string(),
        ));
    }
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(elements)?;

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
fn lt(elements: Vec<Element>) -> Result<Object, EvalError> {
    fold_cmp(elements, |a, b| a < b)
}
fn gt(elements: Vec<Element>) -> Result<Object, EvalError> {
    fold_cmp(elements, |a, b| a > b)
}
fn fold_cmp(elements: Vec<Element>, cmp: fn(Number, Number) -> bool) -> Result<Object, EvalError> {
    if elements.len() < 2 {
        return Err(EvalError::General(
            "application requires at least two argument.".to_string(),
        ));
    }
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(elements)?;
    let first = operands[0];
    let (acc, _) = operands[1..]
        .iter()
        .fold((true, first), |(_, prev), x| (cmp(prev, *x), *x));
    Ok(Object::Bool(acc))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer;
    use crate::object;
    use crate::parser;
    use num::rational::Ratio;

    #[test]
    fn test_eval() {
        use object::Number::Int;
        use object::Number::Rat;
        use object::Object;

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
            ("(begin (+ 2 1) (+ 2 3))", Object::Num(Int(5))),
            ("(begin (+ 2 1))", Object::Num(Int(3))),
        ];
        for (input, expected) in tests.into_iter() {
            let object = eval(parser::parse_program(lexer::lex(input).unwrap()).unwrap()).unwrap();
            assert_eq!(expected, object)
        }
    }
    #[test]
    fn test_eval_ng() {
        let tests = vec![
            //
            ("+", EvalError::NotImplementedSyntax),
            ("(1 1 2)", EvalError::InvalidApplication),
            ("(/ 1 0)", EvalError::ZeroDivision),
            ("(/ 0)", EvalError::ZeroDivision),
        ];

        for (input, expected) in tests.into_iter() {
            let l = lexer::lex(input).unwrap();
            let p = parser::parse_program(l).unwrap();
            match eval(p) {
                Ok(_) => panic!("expected err. but ok."),
                Err(e) => assert_eq!(expected, e),
            }
        }
    }
    #[test]
    fn test_eval_ng_general() {
        let tests = vec![
            //
            ("(< 1)", EvalError::General("fake".to_string())),
            ("(> 1)", EvalError::General("fake".to_string())),
        ];

        for (input, _) in tests.into_iter() {
            let l = lexer::lex(input).unwrap();
            let p = parser::parse_program(l).unwrap();
            match eval(p) {
                Ok(_) => panic!("expected err. but ok."),
                Err(EvalError::General(_)) => { /*ok*/ }
                Err(e) => panic!(format!("expected general error. but [{:?}]", e)),
            }
        }
    }
}
