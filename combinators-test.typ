#import "combinators.typ" : force, delay, comp, partial, cons, conj, apply, constantly, stringify, reduce, reduce_v, result, fail-r, valid, value, tail, map-result, noparse, char-p, char-list, char-not-p, char-not-list, exactly, exactly-as, combine, either, map-p, ignore, iconj, seq, seqf, seqn, or-p, opt, star, plus, stringifing, whole_parser, char-regex-p, skip

#let test(parser, inputs) = {
  for input in inputs {
    let res = parser(input)
    assert(res != none)
    if valid(res) {
      repr(value(res)) + " |" + tail(res) + [\ ]
    } else {
      [failed\ ]
    }
  }
}

#let test-simple(parser) = test(parser, ("a", "a~", "aa", "aab", "b", "b~", "", "x", "x~"))
== char-p
#test-simple(char-p(c => c == "a"))

== char-list
#test-simple(char-list("abc"))

== char-not-list
#test-simple(char-not-list("abc"))

== map
#test-simple(map-p(upper, char-list("abc")))

== ignoring
#test-simple(ignore(char-list("abc")))

#let test-combinations(parser) = test(parser, ("axA~", "axA", "aXA~", "aA", "xA", "yB"))
== either
#test-combinations(either(char-list("a"), char-list("x")))
== combine
=== combine regular
#test-combinations(combine((a, b) => (a, b), char-list("a"), char-list("x")))
=== combine with a delayed
#test-combinations(combine((a, b) => (a, b), delay(() => char-list("a")), char-list("x")))
=== combine with b delayed
#test-combinations(combine((a, b) => (a, b), char-list("a"), delay(() => char-list("x"))))
=== combine with both delayed
#test-combinations(
  combine((a, b) => (a, b), delay(()=>char-list("a")), delay(()=>char-list("x"))),
)

== seq
#test-combinations(seq(char-list("abc"), ignore(char-list("xyz")), char-list("ABC")))
== seqf
#test-combinations(seqf(
  stringify,
  char-list("abc"),
  ignore(char-list("xyz")),
  char-list("ABC"),
))
== seqn
#test-combinations(seqn(1, char-list("abc"), ignore(char-list("xyz")), char-list("ABC")))

== or
#test-simple(or-p(char-list("a"), char-list("b"), char-list("c")))
== opt
#test-simple(opt(char-list("a")))
== star
#test-simple(star(char-list("ab")))
== plus
#test-simple(plus(char-list("ab")))
== stringifying star
#test-simple(stringifing(star(char-list("ab"))))