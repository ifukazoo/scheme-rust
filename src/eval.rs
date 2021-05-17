extern crate num;
use crate::object::Object;
use crate::parser::Atom;
use crate::parser::Element;
use num::rational::Ratio;
use num::rational::Rational64;

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
        Atom::Num(i) => Ok(Object::Rat(Ratio::from_integer(i))),
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
        _ => Err(EvalError::InvalidApplication),
    }
}
fn to_num_vec(elements: Vec<Element>) -> Result<Vec<Rational64>, EvalError> {
    let mut v = vec![];
    for n in elements.into_iter() {
        let obj = eval(n)?;
        if let Object::Rat(r) = obj {
            v.push(r);
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
    Ok(Object::Rat(acc))
}
fn mul(elements: Vec<Element>) -> Result<Object, EvalError> {
    // 引数をi64の配列に変換して集積する
    let operands = to_num_vec(elements)?;
    let acc = operands.iter().product();
    Ok(Object::Rat(acc))
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
        Ok(Object::Rat(-first))
    } else {
        let sum = operands[1..].iter().fold(first, |acc, o| acc - o);
        Ok(Object::Rat(sum))
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
        if divider == Ratio::from_integer(0) {
            Err(EvalError::ZeroDivision)
        } else {
            Ok(Object::Rat(Ratio::from_integer(1) / divider))
        }
    } else {
        let mut sum = operands[0];
        for &r in operands[1..].iter() {
            if r == Ratio::from_integer(0) {
                return Err(EvalError::ZeroDivision);
            }
            sum /= r;
        }
        Ok(Object::Rat(sum))
    }
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
        use object::Object;
        use object::Object::Nil;
        use object::Object::Rat;

        let tests = vec![
            //
            ("1", Rat(Ratio::from_integer(1))),
            ("()", Nil),
            ("(+ 1 2 )", Rat(Ratio::from_integer(3))),
            ("(+ 1 2 3 4 5 6 7 8 9 10)", Rat(Ratio::from_integer(55))),
            ("(+ 1)", Rat(Ratio::from_integer(1))),
            ("(+)", Rat(Ratio::from_integer(0))),
            //
            ("-1", Rat(Ratio::from_integer(-1))),
            ("(+ +1 2)", Rat(Ratio::from_integer(3))),
            ("(+ 0 -1)", Rat(Ratio::from_integer(-1))),
            (
                "(+ +1 +2 +3 +4 +5 +6 +7 +8 +9 +10)",
                Rat(Ratio::from_integer(55)),
            ),
            (
                "(+ -1 -2 -3 -4 -5 -6 -7 -8 -9 -10)",
                Rat(Ratio::from_integer(-55)),
            ),
            //
            ("(- 1)", Rat(Ratio::from_integer(-1))),
            ("(- 0 1 )", Rat(Ratio::from_integer(-1))),
            ("(- 1 2)", Rat(Ratio::from_integer(-1))),
            ("(- 1 2 3 4 5 6 7 8 9 10)", Rat(Ratio::from_integer(-53))),
            ("(- +2 +2 +2)", Rat(Ratio::from_integer(-2))),
            //
            ("(+ (+ 1 2) 3)", Rat(Ratio::from_integer(6))),
            ("(- (+ (+ 1 2) 3) 5)", Rat(Ratio::from_integer(1))),
            //
            ("(*)", Rat(Ratio::from_integer(1))),
            ("(* 1)", Rat(Ratio::from_integer(1))),
            ("(* 0 1 )", Rat(Ratio::from_integer(0))),
            ("(* 1 2)", Rat(Ratio::from_integer(2))),
            ("(* 2 2)", Rat(Ratio::from_integer(4))),
            ("(* 2 -2)", Rat(Ratio::from_integer(-4))),
            ("(* -2 2)", Rat(Ratio::from_integer(-4))),
            ("(* -2 -2)", Rat(Ratio::from_integer(4))),
            ("(* 2 3 4)", Rat(Ratio::from_integer(24))),
            ("(+ (* 2 3) 5)", Rat(Ratio::from_integer(11))),
            //
            ("(/ 1 2 )", Object::Rat(Ratio::new(1, 2))),
            ("(/ 4 2)", Rat(Ratio::from_integer(2))),
            ("(/ 1)", Rat(Ratio::from_integer(1))),
            ("(/ 2)", Rat(Ratio::new(1, 2))),
            ("(/ 1 2 3 4 5)", Object::Rat(Ratio::new(1, 120))),
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
}
