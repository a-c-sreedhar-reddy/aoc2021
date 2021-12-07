let fuelTaken = (crabPositions, finalPosition) => {
  crabPositions->Js.Array2.reduce(
    (acc, position) => acc + Js.Math.abs_int(position - finalPosition),
    0,
  )
}

let fuelTaken2 = (crabPositions, finalPosition) => {
  crabPositions->Js.Array2.reduce((acc, position) => {
    let n = Js.Math.abs_int(position - finalPosition)
    acc + n * (n + 1) / 2
  }, 0)
}

let crabPositions =
  Utils.getInputLines("src/input/Day07.txt")[0]
  ->Js.String2.split(",")
  ->Js.Array2.map(a => a->Belt_Int.fromString->Belt_Option.getWithDefault(0))

crabPositions
->Js.Array2.map(position => fuelTaken(crabPositions, position))
->Js.Math.minMany_int
->Js.log

Belt.Array.make(1958, 0)
->Js.Array2.mapi((_, position) => fuelTaken2(crabPositions, position))
->Js.Math.minMany_int
->Js.log
