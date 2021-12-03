let inputLines = Utils.getInputLines("src/input/Day03.txt")
let numberOfDigits = Js.String.length(inputLines[0])

let convertBinaryArrayToNumber = binaryArray => {
  binaryArray->Js.Array2.reduce((acc, curr) => {
    if curr === 1 {
      acc * 2 + 1
    } else {
      acc * 2
    }
  }, 0)
}

let inputLineDigits =
  inputLines->Js.Array2.map(line =>
    line
    ->Js.String2.split("")
    ->Js.Array2.map(digit => Belt.Int.fromString(digit)->Belt.Option.getWithDefault(0))
  )

let onesCount = inputLineDigits->Js.Array2.reduce((acc, curr) => {
  curr->Js.Array2.forEachi((digit, index) =>
    if digit === 1 {
      acc[index] = acc[index] + 1
    }
  )
  acc
}, Array.make(numberOfDigits, 0))

let gammaRateBinary = onesCount->Js.Array2.map(count => {
  if count >= inputLines->Js.Array2.length / 2 {
    1
  } else {
    0
  }
})

let gammaRate = gammaRateBinary->convertBinaryArrayToNumber

let epsilotRate =
  gammaRateBinary
  ->Js.Array2.map(digit =>
    if digit === 1 {
      0
    } else {
      1
    }
  )
  ->convertBinaryArrayToNumber

Js.log(gammaRate * epsilotRate)

let getOnesCountAtIndex = (list, index) => {
  list->Js.Array2.filter(cur => cur[index] === 1)->Js.Array2.length
}

let oxygen = gammaRateBinary->Js.Array2.reducei((acc, _cur, index) => {
  if acc->Js.Array2.length === 1 {
    acc
  } else {
    let onesCount = getOnesCountAtIndex(acc, index)
    if onesCount >= acc->Js.Array2.length - onesCount {
      acc->Js.Array2.filter(digits => digits[index] === 1)
    } else {
      acc->Js.Array2.filter(digits => digits[index] === 0)
    }
  }
}, inputLineDigits)

let co = gammaRateBinary->Js.Array2.reducei((acc, _cur, index) => {
  if acc->Js.Array2.length === 1 {
    acc
  } else {
    let onesCount = getOnesCountAtIndex(acc, index)
    if onesCount >= acc->Js.Array2.length - onesCount {
      acc->Js.Array2.filter(digits => digits[index] === 0)
    } else {
      acc->Js.Array2.filter(digits => digits[index] === 1)
    }
  }
}, inputLineDigits)

Js.log(oxygen[0]->convertBinaryArrayToNumber * co[0]->convertBinaryArrayToNumber)
