# Pieuvre, Lambda Calculus Proof Assistant

## Overview

Pieuvre is a minimal proof assistant based on the simply typed lambda calculus, illustrating the connection between programs and logic (Curry–Howard correspondence).

## Features
- Lambda terms: abstraction, application, variables, uples, True, ExFalso, Case 
- β-reduction (evaluation)
- α-conversion (renaming variables)
- Logical operators: implication (->), conjunction (/\\), disjunction (\\/), negation (~), True, False
- Simple type system with base types and logical operators
- Type checking and inference
- Proof tactics: intro, intros, apply, exact, trivial, cut, split, left, right, destruct, absurd, exfalso, admit
- Interactive and batch proof modes

## Usage
- `dune exec bin/pieuvre.exe -reduce <file.lam>`: print the reduction sequence for a lambda term.
- `dune exec bin/pieuvre.exe -alpha <file.lam>`: check whether two lambda terms are alpha-convertible.
- `dune exec bin/pieuvre.exe -typecheck <file.lam>`: check whether a term has the given type.
- `dune exec bin/pieuvre.exe <file.8pus>`: run a proof script.
- `dune exec bin/pieuvre.exe`: start the interactive proof REPL.

## Proof scripts
- Proof scripts use the `.8pus` extension.
- A single file can contain multiple proofs, each starting with `Goal ... .` and ending with `Qed.`.
- `Show Proof.` displays the current proof term while a proof is in progress.

## Examples

### Conjunction commutativity
```
Goal A/\B -> B/\A.
intro H.
destruct H.
split.
trivial.
trivial.
Qed.
```

### Higher-order function application
```
Goal A -> B -> (A -> B -> C) -> C.
intros HA HB H.
apply H.
exact HA.
exact HB.
Qed.
```

### Double negation introduction
```
Goal A -> ~(~A).
intros HA HNA.
absurd A.
exact HA.
exact HNA.
Qed.
```

### Disjunction commutativity
```
Goal A\/B -> B\/A.
intros.
destruct H.
right.
trivial.
left.
trivial.
Qed.
```
