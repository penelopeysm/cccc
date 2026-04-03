C compiler compiling C (following Nora Sandler's *Writing a C Compiler*).

# Why the name?

I tried to call it `cc`, but that led to a ton of clashes with the system's existing `cc`.

# Setup

You'll need a working installation of opam (Homebrew works for this).

```bash
git clone --recurse-submodules git@github.com:penelopeysm/cc.git
```

```bash
opam install . --deps-only
eval $(opam env)
dune build
```

There's a small shell script to run the tests:

```bash
./runtests --chapter 1
```

where any arguments are just passed on to the test runner (inside `writing-a-c-compiler-tests/`).

# Extra stuff

I'm trying to take notes as I go along. These will be stored in [`NOTES.md`](./NOTES.md).
