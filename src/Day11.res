let inputs =
  Utils.getInputLines("src/input/Day11.txt")->Js_array2.map(line =>
    Js.String2.split(line, "")->Js_array2.map(numberString =>
      Belt_Int.fromString(numberString)->Belt_Option.getWithDefault(0)
    )
  )

let step = inputs => {
  inputs->Js_array2.map(row => row->Js_array2.map(element => element + 1))
}

let rec flash = inputs => {
  let duplicateArray = array => array->Js_array2.map(row => Js_array2.map(row, element => element))
  let increment = (array, rowIndex, colIndex) => {
    switch Belt_Array.get(array, rowIndex) {
    | None => ()
    | Some(row) =>
      switch Belt_Array.get(row, colIndex) {
      | None => ()
      | Some(0) => ()
      | Some(element) => array[rowIndex][colIndex] = element + 1
      }
    }
  }
  let finalInput = duplicateArray(inputs)
  let flashes = ref(0)
  inputs->Js_array2.forEachi((row, rowIndex) =>
    row->Js_array2.forEachi((element, colIndex) => {
      if element >= 10 {
        increment(finalInput, rowIndex - 1, colIndex - 1)
        increment(finalInput, rowIndex - 1, colIndex)
        increment(finalInput, rowIndex - 1, colIndex + 1)
        increment(finalInput, rowIndex, colIndex - 1)
        finalInput[rowIndex][colIndex] = 0
        increment(finalInput, rowIndex, colIndex + 1)
        increment(finalInput, rowIndex + 1, colIndex - 1)
        increment(finalInput, rowIndex + 1, colIndex)
        increment(finalInput, rowIndex + 1, colIndex + 1)
        flashes.contents = flashes.contents + 1
      } else {
        ()
      }
    })
  )
  switch flashes.contents {
  | 0 => (0, finalInput)
  | _ => {
      let (currentCount, finalInput) = flash(finalInput)
      (flashes.contents + currentCount, finalInput)
    }
  }
}

let steps = 100

let (flashes, finalArray) =
  Belt_Array.make(steps, 0)->Js_array2.reduce(((flashes, currentArray), _) => {
    let steppedArray = step(currentArray)
    let (currentFlashes, finalArray) = flash(steppedArray)
    (currentFlashes + flashes, finalArray)
  }, (0, inputs))

Js.log(flashes)

let rec getAllFlashTime = octopuses => {
  let didAllFlash = octopuses => {
    octopuses->Js.Array2.every(octopusesRow =>
      octopusesRow->Js.Array2.every(octopus => octopus === 0)
    )
  }
  if didAllFlash(octopuses) {
    0
  } else {
    let (_, octopuses) = step(octopuses)->flash
    1 + getAllFlashTime(octopuses)
  }
}

Js.log(getAllFlashTime(inputs))
