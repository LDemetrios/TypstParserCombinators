// Utility functions (mostly clojure-core rewritten)
#let force(x) = if (
  type(x) == dictionary and x.at("very-special-key") == "delay"
) { (x.data)() } else { x }
#let delay(x) = (very-special-key: "delay", data: x)

#let comp(f, g) = it => f(g(it))
#let partial(f, ..x) = (..y) => f(..x, ..y)
#let cons(x, y) = (x, ..y)
#let conj(x, y) = (..x, y)
#let apply(f, ..args, argsColl) = f(..args, ..argsColl)
#let constantly(x) = (..y) => x
#let stringify(..values) = for v in values.pos() { v }

#let reduce(f, coll) = {
  if coll.len() == 0 {
    f()
  } else if coll.len() == 1 {
    coll.at(0)
  } else {
    let x = coll.at(0)
    let i = 1
    while i < coll.len() {
      x = f(x, coll.at(i))
      i += 1
    }
    x
  }
}
#let reduce_v(f, val, coll) = {
  let x = val
  let i = 0
  while i < coll.len() {
    x = f(x, coll.at(i))
    i += 1
  }
  x
}

// Basic result manipulations
#let result(value, tail) = (true, value, tail)
#let fail-r() = (false, none, none)
#let valid(res) = res.at(0)
#let value(res) = res.at(1)
#let tail(res) = res.at(2)
#let map-result(f) = res => if valid(res) {
  result(f(value(res)), tail(res))
} else { res }

// Simple parsers
#let noparse(value) = s => result(value, s)
#let char-p(predicate) = s => {
  if (s.len() > 0 and predicate(s.first())) {
    result(s.first(), s.slice(1))
  } else { fail-r() }
}
#let char-list(s) = char-p(c => c in s)
#let char-not-p(predicate) = char-p(c => not predicate(c))
#let char-not-list(s) = char-p(c => c not in s)
#let exactly(s) = inp => {
  if (inp.starts-with(s)) {
    result(s, inp.slice(s.len()))
  } else {
    fail-r()
  }
}
#let exactly-as(s, v) = inp => {
  if (inp.starts-with(s)) {
    result(v, inp.slice(s.len()))
  } else {
    fail-r()
  }
}

// Basic combinators
#let combine(f, a, b) = s => {
  let ar = force(a)(s)
  if valid(ar) {
    let br = force(b)(tail(ar))
    map-result(valueb => f(value(ar), valueb))(br)
  } else { ar }
}

#let either(a, b) = s => {
  let ar = force(a)(s)
  if valid(ar) { ar } else { force(b)(s) }
}

#let map-p(f, parser) = comp(map-result(f), parser)

// Ignoring
#let ignored = "abracadabra"
#let ignore = partial(map-p, constantly(ignored))
#let iconj(coll, value) = if (value == ignored) {
  coll
} else {
  conj(coll, value)
}

// Sequences
#let seq(..parsers) = reduce_v(partial(combine, iconj), noparse(()), parsers.pos())
#let seqf(f, ..parsers) = map-p(partial(apply, f), apply(seq, parsers.pos()))
#let seqn(n, ..parsers) = apply(seqf, (..vs) => vs.pos().at(n), parsers.pos())

// Grammar constructions
#let or-p(parser, ..parsers) = reduce_v(either, parser, parsers.pos())
#let opt(parser) = or-p(parser, noparse(none))
#let star(parser) = s => {
  let coll = ()
  let rem = s
  let res = parser(s)
  while valid(res) {
    coll.push(value(res))
    rem = tail(res)
    res = parser(rem)
  }
  result(coll, rem)
}
#let plus(parser) = seqf(cons, parser, star(parser))

// Representations
#let stringifing(parser) = map-p(partial(apply, stringify), parser)

#let whole_parser(parser) = s => {
  let res = parser(s)
  if not valid(res) {
    "Couldn't parse: " + s
  } else if "" != tail(res) {
    "Remained: " + tail(res)
  } else {
    value(res)
  }
}

#let char-regex-p(reg) = char-p(it => it.match(regex(reg)) != none)
#let skip(s) = ignore(exactly(s))
