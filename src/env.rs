use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// 環境参照
pub type RefEnv = Rc<RefCell<Environment>>;

/// 環境のマップ
#[derive(Debug, PartialEq)]
pub struct Environment {
    map: HashMap<String, Object>,
    outer: Option<RefEnv>,
}
/// 新しい環境作成
pub fn new_env(map: HashMap<String, Object>) -> RefEnv {
    Rc::new(RefCell::new(Environment { map, outer: None }))
}

/// 値の格納
pub fn set_value(env: &RefEnv, key: &str, value: Object) {
    env.borrow_mut().map.insert(key.to_string(), value);
}

/// 値の取得
pub fn get_value(env: &RefEnv, key: &str) -> Option<Object> {
    match env.borrow().map.get(key) {
        Some(v) => Some(v.clone()),
        // 外の環境を一階層再帰的に参照
        None => match &env.borrow().outer {
            Some(outer) => get_value(&outer, key),
            None => None,
        },
    }
}

/// この環境の外側に一階層環境を追加する.
pub fn add_outer(inner: &RefEnv, outer_env: &RefEnv) {
    inner.borrow_mut().outer = Some(outer_env.clone());
}
#[cfg(test)]
mod test {
    use super::super::object::Number;
    use super::*;
    #[test]
    fn test_get_get_set() {
        let e = new_env(HashMap::new());
        set_value(&e, "key_bool", Object::Bool(true));
        assert_eq!(get_value(&e, "key_bool").unwrap(), Object::Bool(true));

        set_value(&e, "key_int", Object::Num(Number::Int(1)));
        assert_eq!(
            get_value(&e, "key_int").unwrap(),
            Object::Num(Number::Int(1))
        );

        assert_eq!(get_value(&e, "key_none"), None);
    }

    #[test]
    fn test_set_outer() {
        let mut g = HashMap::new();
        g.insert("TRUE".to_string(), Object::Bool(true));
        let global = new_env(g);

        let mut n = HashMap::new();
        n.insert("true".to_string(), Object::Bool(true));
        let local = new_env(n);
        add_outer(&local, &global);
        assert_eq!(get_value(&local, "TRUE").unwrap(), Object::Bool(true));
    }
}
