use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// 環境参照
pub type Env = Rc<RefCell<ObjMap>>;

/// 環境のマップ
#[derive(Debug)]
pub struct ObjMap {
    map: HashMap<String, Object>,
    outer: Option<Env>,
}
/// 新しい環境作成
pub fn new_env(map: HashMap<String, Object>) -> Env {
    Rc::new(RefCell::new(ObjMap { map, outer: None }))
}

/// 値の格納
pub fn set_value(env: &Env, key: &str, value: Object) {
    env.borrow_mut().map.insert(key.to_string(), value);
}

/// 値の取得
pub fn get_value(env: &Env, key: &str) -> Option<Object> {
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
pub fn add_outer(inner: &Env, outer_env: &Env) {
    inner.borrow_mut().outer = Some(outer_env.clone());
}

/// 2つの環境が同じものであるか
pub fn equals(lhs: &Env, rhs: &Env) -> bool {
    let lenv = lhs.borrow();
    let renv = rhs.borrow();

    // outerの一致
    let eq_outer = if let Some(lo) = &lenv.outer {
        if let Some(ro) = &renv.outer {
            equals(lo, ro)
        } else {
            false
        }
    } else {
        // None
        renv.outer.is_none()
    };

    if !eq_outer {
        false
    } else {
        std::ptr::eq(&lenv.map, &renv.map)
    }
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

    #[test]
    fn test_equals() {
        let e = new_env(HashMap::new());
        set_value(&e, "key1", Object::Bool(true));
        let e2 = e.clone();
        assert_eq!(true, equals(&e, &e2));

        set_value(&e, "key2", Object::Bool(false));
        assert_eq!(true, equals(&e, &e2));

        let f = new_env(HashMap::new());
        add_outer(&f, &e);
        assert_eq!(false, equals(&e, &f));
    }
}
