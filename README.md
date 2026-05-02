# mini-c-compiler

A compiler for a subset of C, written in Haskell. Built incrementally as part of CS480 (Compiler Construction).

## Current status

- **Scanner (lexer)** — complete

## Supported language

The scanner recognizes the following tokens:

**Keywords:** `else`, `if`, `int`, `return`, `void`, `while`

**Operators:** `+`, `-`, `*`, `/`, `=`, `==`, `!=`, `<`, `<=`, `>`, `>=`

**Punctuation:** `;`, `,`, `(`, `)`, `[`, `]`, `{`, `}`

**Identifiers:** sequences of alphabetic characters not matching a keyword

**Integer literals:** sequences of decimal digits

**Comments:** `/* ... */` block comments (stripped during scanning)

## Building

Requires [Stack](https://docs.haskellstack.org/).

```bash
stack build
```

## Running

```bash
stack run
```

## Testing

```bash
stack test
```

## Project structure

```
src/
  Scanner.hs   # Lexer: string -> [Token]
  Lib.hs       # Placeholder for future passes
app/
  Main.hs      # Entry point
test/
  ScannerSpec.hs  # Hspec tests for the scanner
```
