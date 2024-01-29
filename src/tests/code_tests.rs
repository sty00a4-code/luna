#[test]
fn counter() {
    use crate::{run, LunaArgs};

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
    dbg!(run(text, &LunaArgs::default())).expect("error happened");
}