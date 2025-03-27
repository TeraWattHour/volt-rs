#[macro_export]
macro_rules! extract {
    ($e:expr, $p:pat) => {
        let $p = &*$e.inner else {
            unreachable!()
        };
    };
}