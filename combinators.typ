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
#let result(value, tail) = (valid:true, value:value, tail:tail)
#let fail-r = (valid:false, value:none, tail:none)
#let map-result(f) = res => if res.valid {
  result(f(res.value), res.tail)
} else { res }

// Simple parsers
#let noparse(value) = s => result(value, s)
#let char-p(predicate) = s => {
  if (s.len() > 0 and predicate(s.first())) {
    result(s.first(), s.slice(1))
  } else { fail-r }
}
#let char-list(s) = char-p(c => c in s)
#let char-not-p(predicate) = char-p(c => not predicate(c))
#let char-not-list(s) = char-p(c => c not in s)
#let exactly(s) = inp => {
  if (inp.starts-with(s)) {
    result(s, inp.slice(s.len()))
  } else {
    fail-r
  }
}
#let exactly-as(s, v) = inp => {
  if (inp.starts-with(s)) {
    result(v, inp.slice(s.len()))
  } else {
    fail-r
  }
}

// Basic combinators
#let combine(f, a, b) = s => {
  let ar = force(a)(s)
  if ar.valid {
    let br = force(b)(ar.tail)
    map-result(valueb => f(ar.value, valueb))(br)
  } else { ar }
}

#let either(a, b) = s => {
  let ar = force(a)(s)
  if ar.valid { ar } else { force(b)(s) }
}

#let map-p(f, parser) = comp(map-result(f), parser)

// Ignoring
#let ignored = "abracadabra"
#let ignore(parser) = map-p(constantly(ignored), parser)
#let iconj(coll, value) = if (value == ignored) {
  coll
} else {
  conj(coll, value)
}

// Sequences
#let seq(..parsers) = reduce_v(partial(combine, iconj), noparse(()), parsers.pos())
#let seqf(f, ..parsers) = map-p(partial(apply, f), seq(..parsers))
#let seqn(n, ..parsers) = seqf((..vs) => vs.pos().at(n), ..parsers)

// Grammar constructions
#let or-p(parser, ..parsers) = reduce_v(either, parser, parsers.pos())
#let opt(parser) = or-p(parser, noparse(none))
#let star(parser) = s => {
  let coll = ()
  let rem = s
  let res = parser(s)
  while res.valid {
    coll.push(res.value)
    rem = res.tail
    res = parser(rem)
  }
  result(coll, rem)
}
#let plus(parser) = seqf(cons, parser, star(parser))

// Representations
#let stringifing(parser) = map-p(partial(apply, stringify), parser)

#let whole_parser(parser) = s => {
  let res = parser(s)
  if not res.valid {
    "Couldn't parse: " + s
  } else if "" != res.tail {
    "Remained: " + res.tail
  } else {
    res.value
  }
}

#let char-regex-p(reg) = char-p(it => it.match(regex(reg)) != none)
#let skip(s) = ignore(exactly(s))
