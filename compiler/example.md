# Example
Parse from a string:

```
ocaml absyn.cmo parser.cmo lexer.cmo interpreter.cmo
Interpreter.eval (Interpreter.from_string "(+ 1 2)");;
```
