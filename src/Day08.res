let inputNumbers =
  Utils.getInputLines("src/input/Day08.txt")
  ->Js.Array2.map(line => {
    Js.String2.split(line, "|")[1]
  })
  ->Js.Array2.map(output => output->Js.String2.split(" ")->Js.Array2.filter(a => a != ""))

let part1 = inputNumbers->Js.Array2.reduce((acc, line) => {
  acc +
  line
  ->Js.Array2.filter(number =>
    switch Js.String2.length(number) {
    | 2 | 4 | 3 | 7 => true
    | _ => false
    }
  )
  ->Js.Array2.length
}, 0)

Js.log(part1)
