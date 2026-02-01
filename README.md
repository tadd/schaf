Schaf
=====

A [Scheme] engine that aims[^1] for [R<sup>5</sup>RS] compliance. Also, a study or an
étude for my daily hacking exercise.

You'll find almost nothing unique or interesting here; this is just one of those
plain and old-fashioned interpreters in C (C23 with some extensions).

You can use this under [MIT license](./LICENSE.md).

Our milestones will be available at the [project
page](https://github.com/users/tadd/projects/3).

## (Very few) Features

* **Selectable GC Algorithms**
  * You can choose the GC algorithm using the `--gc=<epsilon|mark-sweep|...>` option at
    startup.
* **Helpful Error Messages**
  * If an error occurred, a (somewhat) helpful error message will be shown.

```scheme
$ cat sample.scm
(define (f) (g))
(define (g) x)
(f)
$ schaf sample.scm
error: unbound variable: x
(define (g) x)
            ^
	sample.scm:2:13 in 'g'
	sample.scm:1:14 in 'f'
	sample.scm:3:2 in <toplevel>
```

## Why that name?

* Both "**Sch**eme" and "**Sch**af" begin with `Sch`. So we can use the prefix `sch_` for
  our exported symbols in both senses.
* [It](https://de.wiktionary.org/wiki/Schaf)'s like me who got lost.

## Related Works

* [libscary]: A (scary) scalable array library in C.
  * Dynamic-extended, type-safe-on-push, and zero-overhead on read/write

[Scheme]: https://www.scheme.org
[R<sup>5</sup>RS]: https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/
[libscary]: https://github.com/tadd/libscary

[^1]: just a little[^2] bit
[^2]: really
