let input = Utils.getInputLines("src/input/Day15.txt")

let extendRight = (map: array<array<int>>, times) => {
  map->Js.Array2.map(row => {
    Belt_Array.makeBy(times - 1, a => a + 1)->Js.Array2.reduce((acc, cur) => {
      acc->Js.Array2.concat(
        row->Js.Array2.map(element => {
          let newElement = (element + cur)->mod(9)
          newElement == 0 ? 1 : newElement
        }),
      )
    }, row)
  })
}

let extendDown = (map: array<array<int>>, times) => {
  Belt_Array.makeBy(times - 1, a => a + 1)->Js.Array2.reduce((acc, cur) => {
    acc->Js.Array2.concat(
      map->Js.Array2.map(row => {
        row->Js.Array2.map(element => {
          let newElement = (element + cur)->mod(9)
          newElement == 0 ? 1 : newElement
        })
      }),
    )
  }, map)
}

let map1 =
  input
  ->Js.Array2.map(row =>
    row
    ->Js.String2.split("")
    ->Js.Array2.map(element => {
      element->Belt.Int.fromString->Belt_Option.getUnsafe
    })
  )
  ->extendRight(1)
  ->extendDown(1)
  ->Js.Array2.map(row => row->Js.Array2.map(a => a->BigInt.make))
let map2 =
  input
  ->Js.Array2.map(row =>
    row
    ->Js.String2.split("")
    ->Js.Array2.map(element => {
      element->Belt.Int.fromString->Belt_Option.getUnsafe
    })
  )
  ->extendRight(5)
  ->extendDown(5)
  ->Js.Array2.map(row => row->Js.Array2.map(a => a->BigInt.make))
type heuristic = {d: BigInt.t, h: BigInt.t}
let printShortPath = map => {
  let universalSet = []
  let ilen = map->Js_array2.length - 1
  let jlen = map[0]->Js.Array2.length - 1
  for i in 0 to map->Js_array2.length - 1 {
    for j in 0 to map[0]->Js.Array2.length - 1 {
      let _ = universalSet->Js_array2.push((
        j,
        i,
        {
          d: i === 0 && j === 0 ? BigInt.make(0) : Js.Int.max->BigInt.make,
          h: (jlen - j + ilen - i)->BigInt.make,
        },
      ))
    }
  }
  // Js.log(universalSet)
  let rec getShortPath = (universalSet: Js.Array2.t<(int, int, heuristic)>) => {
    let current = universalSet->Js.Array2.reduce((acc, cur) => {
        // Js.log(("acc", acc))
        switch acc {
        | None => Some(cur)
        | Some(acc) => {
            let (_, _, {d: accd, h: acch}) = acc
            // Js.log(("acc distance", accd + acch))
            let (_, _, {d: curd, h: curh}) = cur
            // Js.log(("cur distance", curd + curh))
            accd->BigInt.add(acch)->BigInt.greaterThan(curd->BigInt.add(curh))
              ? Some(cur)
              : Some(acc)
          }
        }
      }, None)->Belt_Option.getUnsafe

    // Js.log(current)
    let (x, y, {d: distance, h}) = current
    if y === map->Js.Array2.length - 1 && x === map[0]->Js.Array2.length - 1 {
      distance
    } else {
      let universalSet = universalSet->Js.Array2.filter(a => a != current)
      let neighbours = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
      let areTuplesEqual = ((a, b), (c, d)) => a === c && b === d
      let universalSet = universalSet->Js.Array2.map(element => {
        let (cx, cy, {d: cdistance, h: cheuristic}) = element
        if neighbours->Js.Array2.some(neighbour => areTuplesEqual((cx, cy), neighbour)) {
          let value = Belt.Array.get(map, cy)->Belt_Option.flatMap(row => Belt.Array.get(row, cx))
          switch value {
          | Some(value) =>
            if cdistance->BigInt.greaterThan(distance->BigInt.add(value)) {
              (cx, cy, {d: distance->BigInt.add(value), h: cheuristic})
            } else {
              element
            }
          | None => element
          }
        } else {
          element
        }
      })
      getShortPath(universalSet)
    }
  }
  Js.log(getShortPath(universalSet))
}

printShortPath(map1)
printShortPath(map2)
