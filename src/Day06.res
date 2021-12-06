module BigInt = {
  type t

  @val
  external make: int => t = "BigInt"

  let add: (t, t) => t = %raw(`
  function(a, b) {
    return a + b
  }
`)
  let addInt = (t, i) => add(t, make(i))
}

let getFishCount = (input, days) => {
  let fish = Belt_Array.make(9, BigInt.make(0))
  input->Js.Array2.forEach(fishValue => fish[fishValue] = fish[fishValue]->BigInt.addInt(1))
  for _ in 1 to days {
    let zeroedFishCount = fish[0]
    for j in 1 to 8 {
      fish[j - 1] = fish[j]
    }
    fish[6] = fish[6]->BigInt.add(zeroedFishCount)
    fish[8] = zeroedFishCount
  }
  fish->Js.Array2.reduce((acc, curr) => acc->BigInt.add(curr), BigInt.make(0))
}

let input =
  Utils.getInputLines("src/input/Day06.txt")[0]
  ->Js.String2.split(",")
  ->Js.Array2.map(a => a->Belt_Int.fromString->Belt_Option.getWithDefault(0))

Js.log(getFishCount(input, 80))
Js.log(getFishCount(input, 256))
