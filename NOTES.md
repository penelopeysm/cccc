# Chapter 1

If you load up x86-64 clang on Compiler Explorer, it will by default give you Intel syntax. You can change this by clicking on the 'Output...' dropdown and disabling the checkbox for that.

Intel looks like this:

```
main:
        mov     eax, 2
        ret
```

and AT&T:

```
main:
        movl    $2, %eax
        retq
```

On my own laptop, which is an Intel Mac (this is one of the rare times I'm glad I am *not* using an ARM one!), the default invocation of `clang` (on v22.1.2) is not as simplified as in the book.
It contains extra instructions for managing the stack frame.
You can get rid of those by adding `-fomit-frame-pointer` to the list of compiler flags.

On macOS the symbol names (like `main`) need to further be prefixed with an underscore, or else the linker will complain that it can't find `_main`.
