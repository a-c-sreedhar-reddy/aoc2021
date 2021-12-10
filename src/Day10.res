// let input = "{([(<{}[<>[]}>{[]{[(<()>"
let inputs = Utils.getInputLines("src/input/Day10.txt")
let getSyntaxErrorScore = input => {
  let doBracketsMatch = (opened, closed) => {
    switch opened {
    | "(" => closed === ")"
    | "{" => closed === "}"
    | "<" => closed === ">"
    | "[" => closed === "]"
    | _ => false
    }
  }
  let getScore = char =>
    switch char {
    | ")" => 3
    | "]" => 57
    | "}" => 1197
    | ">" => 25137
    | _ => 0
    }
  let stack = []
  input->Js.String2.split("")->Js.Array2.reduce((acc, char) => {
    switch acc {
    | Some(_) => acc
    | None =>
      switch char {
      | "(" | "[" | "{" | "<" => {
          let _ = stack->Js.Array2.push(char)
          None
        }
      | ")" | "]" | "}" | ">" =>
        switch stack->Js.Array2.pop {
        | Some(opened) =>
          if doBracketsMatch(opened, char) {
            None
          } else {
            Some(getScore(char))
          }
        | None => None
        }
      | _ => None
      }
    }
  }, None)->Belt_Option.getWithDefault(0)
}
Js.log(inputs->Js.Array2.reduce((acc, curr) => acc + getSyntaxErrorScore(curr), 0))

module BigInt = {
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
  external toInt: t => int = "%identity"
  let addInt = (t, i) => add(t, make(i))
}
let getIncompleteScore = input => {
  let getScore = char =>
    switch char {
    | ")" => 3
    | "]" => 57
    | "}" => 1197
    | ">" => 25137
    | _ => 0
    }
  let doBracketsMatch = (opened, closed) => {
    switch opened {
    | "(" => closed === ")"
    | "{" => closed === "}"
    | "<" => closed === ">"
    | "[" => closed === "]"
    | _ => false
    }
  }
  let stack = []
  let corrupted = input->Js.String2.split("")->Js.Array2.reduce((acc, char) => {
      switch acc {
      | Some(_) => acc
      | None =>
        switch char {
        | "(" | "[" | "{" | "<" => {
            let _ = stack->Js.Array2.push(char)
            None
          }
        | ")" | "]" | "}" | ">" =>
          switch stack->Js.Array2.pop {
          | Some(opened) =>
            if doBracketsMatch(opened, char) {
              None
            } else {
              let _ = stack->Js.Array2.push(opened)
              Some(getScore(char))
            }
          | None => None
          }
        | _ => None
        }
      }
    }, None)->Belt_Option.isSome
  if !corrupted {
    Some(stack->Belt_Array.reverse->Js.Array2.reduce((acc, cur) => {
        BigInt.multiply(BigInt.make(5), acc)->BigInt.addInt(
          switch cur {
          | "(" => 1
          | "[" => 2
          | "{" => 3
          | "<" => 4
          | _ => 0
          },
        )
      }, BigInt.make(0)))
  } else {
    None
  }
}
let sorted =
  inputs
  ->Js.Array2.map(input => getIncompleteScore(input))
  ->Js_array2.filter(value => value->Belt_Option.isSome)
  ->Js_array2.map(value => value->Belt.Option.getUnsafe)
  ->Belt_SortArray.stableSortBy((a, b) => a->BigInt.subtract(b)->BigInt.toInt)

Js.log(sorted[sorted->Js.Array2.length / 2])
