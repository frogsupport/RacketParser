# RacketParser

A partial parser for a simple calculator language. This program will judge whether the given source code follows the
syntactical rules of the language or not; no parse tree is created. Created by Kyle Schaudt for CS441.

## How to Use

1. Pull this repository down to your local machine.
2. Open the 'main.rkt' file.
3. Enter the file path for the file you wish to parse as the parameter in the `parse` function. **Note**: The file you choose should be in the same directory.
4. The console will print 'Accept' if the source code you gave is syntactically correct. Otherwise it will throw an error with the offending token and line number from the given source code.

### Grammar

The grammar of this language is from <u>Programming Language Pragmatics</u>, by Michael Scott.

The rules of the grammar are as follows:

```
program −→ stmt-list $$

stmt list −→ stmt stmt-list | ε

stmt −→ id := expr | read id | write expr

expr −→ term term-tail

term tail −→ add-op term term-tail | ε

term −→ factor factor-tail

factor tail −→ mult-op factor factor-tail | ε

factor −→ ( expr ) | id | number

add op −→ + | -

mult op −→ * | /
```

### Resources Used

[Beautiful Racket](https://beautifulracket.com/) <br />
[Rosetta Code](https://rosettacode.org/wiki/Sorting_algorithms/Selection_sort#Racket)
