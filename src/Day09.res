let heightMap =
  Utils.getInputLines("src/input/Day09.txt")->Js.Array2.map(row =>
    row
    ->Js.String2.split("")
    ->Belt.Array.map(val => val->Belt.Int.fromString->Belt.Option.getWithDefault(0))
  )
let getValueAtIndex = (heightMap, rowindex, colindex) => {
  open Belt
  heightMap[rowindex]->Belt.Option.mapWithDefault(10, row =>
    row[colindex]->Belt.Option.getWithDefault(10)
  )
}
let part1 = heightMap->Js.Array2.reducei((acc, row, rowindex) => {
  row
  ->Js.Array2.filteri((value, colindex) => {
    let top = getValueAtIndex(heightMap, rowindex - 1, colindex)
    let right = getValueAtIndex(heightMap, rowindex, colindex + 1)
    let bottom = getValueAtIndex(heightMap, rowindex + 1, colindex)
    let left = getValueAtIndex(heightMap, rowindex, colindex - 1)
    [top, right, left, bottom]->Js.Array2.every(adjacentValue => adjacentValue > value)
  })
  ->Js.Array2.reduce((acc, value) => acc + value + 1, acc)
}, 0)
Js.log(part1)

let lowpoints = heightMap->Js.Array2.reducei((acc, row, rowindex) => {
  let newpoints =
    row
    ->Js.Array2.mapi((value, colindex) => {
      let top = getValueAtIndex(heightMap, rowindex - 1, colindex)
      let right = getValueAtIndex(heightMap, rowindex, colindex + 1)
      let bottom = getValueAtIndex(heightMap, rowindex + 1, colindex)
      let left = getValueAtIndex(heightMap, rowindex, colindex - 1)
      if [top, right, left, bottom]->Js.Array2.every(adjacentValue => adjacentValue > value) {
        Some(rowindex, colindex)
      } else {
        None
      }
    })
    ->Js.Array2.filter(a => Belt_Option.isSome(a))
    ->Js.Array2.map(a => Belt_Option.getUnsafe(a))
  acc->Js.Array2.concat(newpoints)
}, [])

let visited = Belt_Array.makeBy(heightMap->Js.Array2.length, _ =>
  Belt_Array.make(heightMap[0]->Js.Array2.length, false)
)
let rec getBasin = (rowindex, colindex) => {
  open Belt
  switch visited[rowindex]
  ->Belt.Option.map(row => row[colindex])
  ->Belt.Option.getWithDefault(None) {
  | Some(true) | None => 0
  | Some(false) =>
    switch (heightMap[rowindex]->Belt_Option.getWithDefault([]))[colindex] {
    | Some(9) | Some(10) | None => 0
    | Some(_) => {
        let row = visited[rowindex]->Belt_Option.getUnsafe

        Belt_Array.setUnsafe(row, colindex, true)
        let final =
          1 +
          getBasin(rowindex - 1, colindex) +
          getBasin(rowindex, colindex + 1) +
          getBasin(rowindex + 1, colindex) +
          getBasin(rowindex, colindex - 1)
        final
      }
    }
  }
}

let basins =
  lowpoints
  ->Js.Array2.map(((rowindex, colindex)) => getBasin(rowindex, colindex))
  ->Js.Array2.sortInPlaceWith((a, b) => b - a)
let basinlenght = basins->Js.Array2.length
Js.log(basins[0] * basins[1] * basins[2])
