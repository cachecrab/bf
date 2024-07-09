# bf

My first project in Zig - a brainfuck interpreter.

Written as an experiment into a different instruction representation and
optimised for cache locality. Also has some kind of run-length encoding.

## Building

Verify nothing funky going on your machine:

```
zig build test
```

Actually build:

```
zig build --release=safe
```

or if you trust me

```
zig build --release=fast
```

## Running

```
Usage: bf <file_path>
```

Samples have been provided in ./samples
