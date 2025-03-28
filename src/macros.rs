#[macro_export]
macro_rules! extract {
    ($e:expr, $p:pat) => {
        let $p = &*$e.inner else {
            unreachable!()
        };
    };
}

#[macro_export]
macro_rules! variant {
    ($e:expr, $p:pat) => {
        let $p = $e else {
            unreachable!()
        };
    };
}