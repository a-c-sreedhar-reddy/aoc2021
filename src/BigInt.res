type t

@val
external make: int => t = "BigInt"

let add: (t, t) => t = %raw(`
  function(a, b) {
    return a + b
  }
`)
let multiply: (t, t) => t = %raw(`
  function(a, b) {
    return a *b
  }
`)
let subtract: (t, t) => t = %raw(`
  function(a, b) {
    return a  - b
  }
`)

let greaterThan: (t, t) => bool = %raw(`
  function(a, b) {
    return a  > b
  }
`)
let lessThan: (t, t) => bool = %raw(`
  function(a, b) {
    return a  < b
  }
`)

external toInt: t => int = "%identity"
let addInt = (t, i) => add(t, make(i))

let max = (values: array<t>) => {
  let value = ref(values[0])
  values->Js.Array2.forEach(v =>
    if v->greaterThan(value.contents) {
      value.contents = v
    }
  )
  value.contents
}

let min = (values: array<t>) => {
  let value = ref(values[0])
  values->Js.Array2.forEach(v =>
    if v->lessThan(value.contents) {
      value.contents = v
    }
  )
  value.contents
}
