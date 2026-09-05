C compiler compiling C (following [Nora Sandler's *Writing a C Compiler*](https://nostarch.com/writing-c-compiler)).

# Why the name?

I tried to call it `cc`, but that led to a ton of clashes with the system's existing `cc`.

# Setup

You'll need a working installation of opam (choose your favourite package manager; for example, Homebrew works for this).

I _think_ that as of 2026-09-05 the code should be compilable with standard OCaml, but at some point in time I'm going to start using [OxCaml](https://oxcaml.org/) features.
(For clarity, I work at Jane Street, and this side project is a small way for me to learn more about what my compiler friends are doing!)
Anyway, it's very likely that when that happens, I'll forget to update the README to mention this.
So if you want to clone this locally, you may as well start using OxCaml.

```bash
git clone --recurse-submodules git@github.com:penelopeysm/cccc.git

# Create a local switch
opam switch create . 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default -y

# Then setup
opam install . --deps-only
eval $(opam env)
dune build
```

To develop, you might want to install extra tooling:

```bash
opam install ocamlformat ocaml-lsp-server merlin utop
```

There's a small shell script to run the tests:

```bash
./runtests ARGS...
```

where any arguments are just passed on to the test runner (inside `writing-a-c-compiler-tests/`).
For example, if the book says to run

```
./test_compiler /path/to/your/compiler ARGS
```

then you can run, in place of it,

```
./runtests ARGS
```

# Web

There's a small website, in `web/`, in which the OCaml code is compiled to JavaScript using `js_of_ocaml` and run in the browser.
(The frontend was vibe coded, I've done enough web apps in my time and I didn't really consider it important to handwrite it :-).)

You can either view it at https://pysm.dev/cccc, or to run it locally, do:

```bash
dune build web/main.js
python3 -m http.server --directory web
```

# Extra stuff

I'm trying to take notes as I go along. These will be stored in [`NOTES.md`](./NOTES.md).
