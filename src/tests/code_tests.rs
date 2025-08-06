use crate::luna_impl::ast::Chunk;

#[test]
fn counter() {
    use crate::run_str;

    let text = r#"
        let fn counter() {
            let counter = 0
            return fn () {
                counter = counter + 1
                return counter
            }
        }
        
        let c1 = counter()
        let c2 = counter()
        assert(c1() == 1)
        assert(c1() == 2)
        assert(c1() == 3)
        assert(c2() == 1)
        assert(c2() == 2)
        assert(c2() == 3)
    "#;
    dbg!(run_str::<Chunk>(text, None)).expect("error happened");
}
#[test]
fn counter_default() {
    use crate::run_str;

    let text = r#"
        let default = 1
        let fn counter() {
            let counter = 0
            return fn () {
                counter = counter + default
                return counter
            }
        }
        
        let c1 = counter()
        let c2 = counter()
        assert(c1() == 1)
        assert(c1() == 2)
        assert(c1() == 3)
        assert(c2() == 1)
        assert(c2() == 2)
        assert(c2() == 3)
    "#;
    dbg!(run_str::<Chunk>(text, None)).expect("error happened");
}