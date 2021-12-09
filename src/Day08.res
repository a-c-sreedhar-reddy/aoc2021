let inputNumbers =
  Utils.getInputLines("src/input/Day08.txt")
  ->Js.Array2.map(line => {
    Js.String2.split(line, "|")
  })
  ->Js.Array2.map(output => (
    output[0]->Js.String2.split(" ")->Js.Array2.filter(a => a != ""),
    output[1]->Js.String2.split(" ")->Js.Array2.filter(a => a != ""),
  ))

let part1 = inputNumbers->Js.Array2.reduce((acc, (_, line)) => {
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

let hasAllLettersOf = (a, b) => {
  b->Js.String2.split("")->Js.Array2.every(letter => a->Js.String2.includes(letter))
}
let findNumbers = input => {
  let one = input->Js.Array2.find(a => a->Js.String2.length === 2)->Belt_Option.getUnsafe
  let four = input->Js.Array2.find(a => a->Js.String2.length === 4)->Belt_Option.getUnsafe
  let seven = input->Js.Array2.find(a => a->Js.String2.length === 3)->Belt_Option.getUnsafe
  let eight = input->Js.Array2.find(a => a->Js.String2.length === 7)->Belt_Option.getUnsafe
  let remaining = input->Js.Array2.filter(a => !([one, four, seven, eight]->Js.Array2.includes(a)))
  let three =
    remaining
    ->Js.Array2.find(a => Js.String2.length(a) === 5 && a->hasAllLettersOf(one))
    ->Belt_Option.getUnsafe
  let remaining = remaining->Js.Array2.filter(a => a != three)
  let nine =
    remaining
    ->Js.Array2.find(a => Js.String2.length(a) === 6 && a->hasAllLettersOf(three))
    ->Belt_Option.getUnsafe
  let remaining = remaining->Js.Array2.filter(a => a != nine)
  let five =
    remaining
    ->Js.Array2.find(a => Js.String2.length(a) === 5 && nine->hasAllLettersOf(a))
    ->Belt_Option.getUnsafe
  let remaining = remaining->Js.Array2.filter(a => a != five)
  let two = remaining->Js.Array2.find(a => Js.String2.length(a) === 5)->Belt_Option.getUnsafe
  let remaining = remaining->Js.Array2.filter(a => a != two)
  let zero =
    remaining
    ->Js.Array2.find(a => Js.String2.length(a) === 6 && a->hasAllLettersOf(one))
    ->Belt_Option.getUnsafe

  let remaining = remaining->Js.Array2.filter(a => a != zero)
  let six = remaining[0]
  [zero, one, two, three, four, five, six, seven, eight, nine]
}

Js.log(part1)

let getCorrespoindingNumber = (numbers, numberString) =>
  numbers->Js.Array2.findIndex(number =>
    Js.String.length(number) === Js.String.length(numberString) &&
      number->hasAllLettersOf(numberString)
  )
let convertDigitsToNumber = digits => digits->Js.Array2.reduce((acc, curr) => acc * 10 + curr, 0)

let part2 = inputNumbers->Js.Array2.reduce((acc, (digits, digitsToDecode)) => {
  let numbers = findNumbers(digits)
  let decodedNumber =
    digitsToDecode
    ->Js.Array2.map(numberString => getCorrespoindingNumber(numbers, numberString))
    ->convertDigitsToNumber
  acc + decodedNumber
}, 0)

Js.log(part2)
