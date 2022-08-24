# Rusty Scheme

The start of a basic scheme compiler written in rust for fun. Currently just a repl, but could be used as a library if one so chooses.

## Usage

Standard cargo project - with rustc and cargo installed:

```bash
# run tests
cargo test
# start repl
cargo run
```


## Completion State
- [x] s brackets and math operations handling
- [x] defining variables and functions
- [x] calling functions
- [ ] conditionals
- [ ] flow control

### Improvements
- [ ] try to minimize copying of data (`Rc` wrap `Symbol`?)
- [ ] more documentation
- [ ] (far off) jit compilation

