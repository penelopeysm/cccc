C compiler compiling C (following Nora Sandler's *Writing a C Compiler*).

# Why the name?

I tried to call it `cc`, but that led to a ton of clashes with the system's existing `cc`.

# Setup

You'll need a working installation of opam (Homebrew works for this).

```bash
git clone --recurse-submodules git@github.com:penelopeysm/cccc.git

# Create a local switch first if you want
opam switch create . 5.4.1 -y

# Then setup
opam install . --deps-only
eval $(opam env)
dune build
```

To develop, you might want to install extra tooling:

```bash
opam install ocamlformat ocaml-lsp-server
```

There's a small shell script to run the tests:

```bash
./runtests --chapter 1
```

where any arguments are just passed on to the test runner (inside `writing-a-c-compiler-tests/`).
For information about what arguments are supported please refer to the textbook.

# Web

There's a small website, in `web/`, in which the OCaml code is compiled to JavaScript using `js_of_ocaml` and run in the browser.

You can either view it at https://pysm.dev/cccc, or to run it locally, do:

```bash
dune build web/main.js
python3 -m http.server --directory web
```

# Extra stuff

I'm trying to take notes as I go along. These will be stored in [`NOTES.md`](./NOTES.md).
