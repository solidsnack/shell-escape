Library for generating fully escaped strings for use with the shell.

Simple usage examples:

```haskell
> bytes (sh "abc def")
"abc' def'"

> bytes (bash "abc def")
"$'abc def'"
```

The first result might seem wrong -- but it is an actual shell literal.
