// Utility functions (mostly clojure-core rewritten)
#let force(x) = if (type(x) == dictionary and x.at("very-special-key") == "delay") { (x.data)() } else { x }
#let delay(x) = (very-special-key: "delay", data: x)

#let comp(f, g) = it => f(g(it))
#let partial(f, ..x) = (..y) => f(..x, ..y)
#let cons(x, y) = (x, ..y)
#let conj(x, y) = (..x, y)
#let apply(f, ..args, argsColl) = f(..args, ..argsColl)
#let constantly(x) = (..y) => x
#let stringify(..values) = for v in values.pos() {
  v
}

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
#let result(value, tail) = (valid: true, value: value, tail: tail)
#let fail-r = (valid: false, value: none, tail: none)
#let map-result(f) = res => if res.valid {
  (valid: true, value: f(res.value), tail: res.tail)
} else { res }

// Simple parsers
#let noparse(value) = s => (valid: true, value: value, tail: s)
#let char-p(predicate) = s => {
  if (s.len() > 0 and predicate(s.first())) {
    (valid: true, value: s.first(), tail: s.slice(1))
  } else { fail-r }
}
#let char-list(list) = s => {
  if (s.len() > 0 and s.first() in list) {
    (valid: true, value: s.first(), tail: s.slice(1))
  } else { fail-r }
}
#let char-not-p(predicate) = s => {
  if (s.len() > 0 and not predicate(s.first())) {
    (valid: true, value: s.first(), tail: s.slice(1))
  } else { fail-r }
}
#let char-not-list(list) = s => {
  if (s.len() > 0 and (s.first() not in list)) {
    (valid: true, value: s.first(), tail: s.slice(1))
  } else { fail-r }
}
#let exactly(s) = inp => {
  if (inp.starts-with(s)) {
    (valid: true, value: s, tail: inp.slice(s.len()))
  } else {
    fail-r
  }
}
#let exactly-as(s, v) = inp => {
  if (inp.starts-with(s)) {
    (valid: true, value: v, tail: inp.slice(s.len()))
  } else {
    fail-r
  }
}

// Basic combinators
#let combine(f, a, b) = s => {
  let ar = force(a)(s)
  if ar.valid {
    let br = force(b)(ar.tail)
    if br.valid {
      (valid: true, value: f(ar.value, br.value), tail: br.tail)
    } else { br }
  } else { ar }
}

#let either(a, b) = s => {
  let ar = force(a)(s)
  if ar.valid { ar } else { force(b)(s) }
}

#let map-p(f, parser) = s => {
  let pre-res = parser(s)
  if pre-res.valid {
    (valid: true, value: f(pre-res.value), tail: pre-res.tail)
  } else { pre-res }
}

// Ignoring
#let ignored = "abracadabra"
#let ignore(parser) = s => {
  let pre-res = parser(s)
  if pre-res.valid {
    (valid: true, value: ignored, tail: pre-res.tail)
  } else { pre-res }
}

#let iconj(coll, value) = if (value == ignored) {
  coll
} else {
  conj(coll, value)
}

// Sequences
#let seq(..parsers) = s => {
  let still-valid = true
  let return-value = ()
  let remainder = s
  for parser in parsers.pos() {
    if still-valid {
      let next = force(parser)(remainder)
      if next.valid {
        remainder = next.tail
        if (next.value != ignored) {
          return-value.push(next.value)
        }
      } else {
        still-valid = false
      }
    }
  }
  if still-valid {
    (valid: true, value: return-value, tail: remainder)
  } else { fail-r }
}

/*reduce_v(partial(combine, iconj), noparse(()), parsers.pos())*/

#let seqf(f, ..parsers) = s => {
  let pre-res = seq(..parsers)(s)
  if pre-res.valid {
    (valid: true, value: f(..pre-res.value), tail: pre-res.tail)
  } else { pre-res }
}
#let seqn(n, ..parsers) = s => {
  let pre-res = seq(..parsers)(s)
  if pre-res.valid {
    (valid: true, value: pre-res.value.at(n), tail: pre-res.tail)
  } else { pre-res }
}

// Grammar constructions
#let or-p(parser, ..parsers) = s => {
  let res = parser(s)
  for p in parsers.pos() {
    if not res.valid {
      res = p(s)
    }
  }
  res
}
#let opt(parser) = s => {
  let res = parser(s)
  if res.valid {
    res
  } else {
    (valid: true, value: none, tail: s)
  }
}
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
#let plus(parser) = s => {
  let res = parser(s)
  if res.valid {
    let rem = res.tail
    let coll = ()
    while res.valid {
      coll.push(res.value)
      rem = res.tail
      res = parser(rem)
    }
    (valid: true, value: coll, tail: rem)
  } else { fail-r }
}

// Representations
#let stringifing(parser) = s => {
  let res = parser(s)
  if res.valid {
    (valid: true, value: stringify(..res.value), tail: res.tail)
  } else { fail-r }
}

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
#let skip(s) = inp => {
  if inp.starts-with(s) {
    (valid: true, value: ignored, tail: inp.slice(s.len()))
  } else { fail-r }
}

#let regex-p(rgx, func: (it)=>it.text) = s => {
  let match-res = s.match(regex("(" + rgx + ").*"))
  if match-res == none or match-res.start != 0 { fail-r } else {
    let captured = match-res.captures.at(0).match(regex(rgx))
    (valid:true, value:func(captured), tail:s.slice(captured.text.len()))
  }
}

