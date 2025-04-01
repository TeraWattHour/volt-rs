## Volt
An imperative language made with Rust and LLVM. This is a toy project to explore the LLVM bindings for Rust (inkwell package).


### Example code
The following statements make the process exit with the status code equal to the 10th number of the Fibonacci sequence.
```rust
fn fibonacci(l: int, r: int, n: int) -> int {
  if n == 0 { return l; }
  return fibonacci(r, l+r, n-1);
}

fn main() -> int {
  return fibonacci(0, 1, 10);  
}
```
