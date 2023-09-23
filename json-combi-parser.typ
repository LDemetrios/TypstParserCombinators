#import "combinators.typ" : delay, comp, partial, cons, conj, apply, constantly, stringify, reduce, reduce_v, result, fail-r, valid, value, tail, map-result, noparse, char-p, char-list, char-not-p, char-not-list, exactly, exactly-as, combine, either, map-p, ignore, iconj, seq, seqf, seqn, or-p, opt, star, plus, stringifing, whole_parser, char-regex-p, skip

#let null-p = exactly-as("null", none)
#let bool-p = or-p(exactly-as("true", true), exactly-as("false", false))
#let letter-p = char-regex-p("[A-Za-z]")
#let digit-p = char-regex-p("[0-9]")
#let space-p = char-regex-p("\\s")
#let ws-p = ignore(star(space-p))
#let number-p = map-p(int, stringifing(plus(digit-p)))
#let identifier-p = stringifing(seqf(cons, letter-p, star(or-p(letter-p, digit-p))))

#let string-p = seqn(0, skip("\""), stringifing(star(char-not-list("\"\\"))), skip("\""))

#let block(begin-c, parser, end-c) = seqn(
  0,
  skip(begin-c),
  opt(seqf(cons, ws-p, parser, star(seqn(0, ws-p, skip(","), ws-p, parser)))),
  ws-p,
  skip(end-c),
)

#let array-p(dict) = block("[", delay(() => dict.at("value")(dict)), "]")
#let member-p(dict) = seq(
  or-p(identifier-p, string-p),
  ws-p,
  skip(":"),
  ws-p,
  delay(() => dict.at("value")(dict)),
)
#let assoc(k, v, dict) = {
  dict.insert(k, v)
  dict
}
#let object-p(dict) = map-p(
  pairs => reduce_v((d, arr) => assoc(arr.at(0), arr.at(1), d), (:), pairs),
  block("{", dict.at("member")(dict), "}"),
)

#let value-p(dict) = or-p(
  null-p,
  bool-p,
  number-p,
  string-p,
  dict.at("array")(dict),
  dict.at("object")(dict),
)
#let json-dict = (
  "array": array-p,
  "member": member-p,
  "object": object-p,
  "value": value-p,
)
#let json-p = whole_parser(seqn(0, ws-p, value-p(json-dict), ws-p))

= test1
#json-p(" [1, {a: \"hello\", b: [1, 2, 3]}, null]")

= test2
#json-p(
`{"widget": {
    "debug": "on",
    "window": {
        "title": "Sample Konfabulator Widget",
        "name": "main_window",
        "width": 500,
        "height": 500
    },
    "image": { 
        "src": "Images/Sun.png",
        "name": "sun1",
        "hOffset": 250,
        "vOffset": 250,
        "alignment": "center"
    },
    "text": {
        "data": "Click Here",
        "size": 36,
        "style": "bold",
        "name": "text1",
        "hOffset": 250,
        "vOffset": 100,
        "alignment": "center",
        "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
    }
}}    `.text
)