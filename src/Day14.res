@send
external flatMap: (array<'a>, 'a => array<'b>) => array<'b> = "flatMap"

module Polymer = {
  type template = array<(char, char)>
  let getMolecules = template => {
    let atoms = template->Js.String2.split("")
    switch atoms->Js.Array2.length {
    | 0 | 1 => ([], "")
    | _ => (atoms->Js_array2.reducei((acc, curr, index) => {
          if index < atoms->Js_array2.length - 1 {
            let molecule = curr ++ atoms[index + 1]
            let isInArray = Js.Array2.find(acc, ((mol, _)) => mol === molecule)
            switch isInArray {
            | Some(_) =>
              Js.Array2.map(acc, ((mol, val)) =>
                mol === molecule ? (mol, val->BigInt.addInt(1)) : (mol, val)
              )
            | None => {
                let _ = Js.Array2.push(acc, (molecule, BigInt.make(1)))
                acc
              }
            }
          } else {
            acc
          }
        }, []), Js.String2.charAt(template, Js.String2.length(template) - 1))
    }
  }
  let toString = template => {
    let final = template->Belt_Array.joinWith("", ((first, _)) => first)
    switch template->Js.Array2.length {
    | 0 => final
    | _ =>
      let (_, last) = template[template->Js.Array2.length - 1]
      final ++ last
    }
  }
  let getRuleFor = (rules, first) => {
    rules->Js.Array2.find(((atom1, atom2, _)) => atom1 ++ atom2 === first)
  }
  let apply = ((atom1, atom2, middle)) => {
    (atom1 ++ middle, middle ++ atom2)
  }
  let rec polymerize = (template: array<(string, BigInt.t)>, ~rules, ~times=1, ()) => {
    if times == 0 {
      template
    } else {
      let addMoleculeOfCount = (template, molecule, count: BigInt.t) => {
        switch Js.Array2.findIndex(template, ((first, _)) => molecule === first) {
        | -1 =>
          let _ = template->Js.Array2.push((molecule, count))
        | index => {
            let (molecule, c) = template[index]
            template[index] = (molecule, BigInt.add(count, c))
          }
        }
      }
      let newTemplate = []
      template->Js_array2.forEach(((molecule, count)) =>
        switch getRuleFor(rules, molecule) {
        | Some(rule) => {
            let (first, second) = apply(rule)
            addMoleculeOfCount(newTemplate, first, count)
            addMoleculeOfCount(newTemplate, second, count)
          }
        | None => addMoleculeOfCount(newTemplate, molecule, count)
        }
      )
      polymerize(newTemplate, ~rules, ~times=times - 1, ())
    }
  }
}

let input = Utils.getInputLines("src/input/Day14.txt")
let template = input[0]

let rules = Belt.Array.sliceToEnd(input, 2)->Js_array2.map(line => {
  let rule = line->Js.String2.split(" -> ")
  let molecule = rule[0]->Js.String2.split("")
  let middle = rule[1]
  (molecule[0], molecule[1], middle)
})

let getCount = (template: array<(string, BigInt.t)>) => {
  template
  ->Js.Array2.map(((string, count)) => ((string->Js_string2.split(""))[0], count))
  ->Js.Array2.reduce((acc, (cur, count)) => {
    let index = acc->Js.Array2.findIndex(((val, _)) => val === cur)
    switch index {
    | -1 =>
      let _ = Js.Array2.push(acc, (cur, count))
    | _ =>
      let (cur, val) = acc[index]
      acc[index] = (cur, val->BigInt.add(count))
    }
    acc
  }, [])
}
let (molecules, lastChar) = Polymer.getMolecules(template)

let polymer = molecules->Polymer.polymerize(~rules, ~times=10, ())
let count =
  polymer
  ->getCount
  ->Js_array2.map(((cur, count)) => cur === lastChar ? count->BigInt.addInt(1) : count)

let max = BigInt.max(count)
let min = BigInt.min(count)

Js.log(max->BigInt.subtract(min))

let polymer = molecules->Polymer.polymerize(~rules, ~times=40, ())
let count =
  polymer
  ->getCount
  ->Js_array2.map(((cur, count)) => cur === lastChar ? count->BigInt.addInt(1) : count)

let max = BigInt.max(count)
let min = BigInt.min(count)

Js.log(max->BigInt.subtract(min))
