#[macro_export]
macro_rules! node {
    ($e:expr, $p:pat) => {
        let $p = &$e.node.1 else { unreachable!() };
    };
}

#[macro_export]
macro_rules! stmt {
    ($e:expr, $p:pat) => {
        let $p = &$e.1 else { unreachable!() };
    };
}

#[macro_export]
macro_rules! variant {
    ($e:expr, $p:pat) => {
        let $p = $e else { unreachable!() };
    };
}
