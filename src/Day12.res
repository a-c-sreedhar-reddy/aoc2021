module Cave = {
  type rec t = {id: string, paths: array<string>}
  let make = id => {id: id, paths: []}
  let getEdgeCaves = (caves, cave: t) => {
    cave.paths->Js.Array2.map(caveId =>
      caves->Js.Array2.find(cave => cave.id === caveId)->Belt_Option.getUnsafe
    )
  }
  let isEndCave = cave => {
    cave.id === "end"
  }
  let isVisited = (caves: array<t>, cave: t) => {
    caves->Js.Array2.includes(cave)
  }
  let isSmall = (cave: t) => {
    cave.id->Js.String2.toLowerCase === cave.id
  }
  let visit = (visitedCaves: array<t>, cave: t) => {
    Js_array2.concat(visitedCaves, [cave])
  }
  let doesExist = (caves, id) => {
    caves->Js.Array2.some(cave => cave.id === id)
  }
}

let rec getPathsCount = (caves, startingCave, visitedCaves) => {
  let edgeCaves = Cave.getEdgeCaves(caves, startingCave)
  let visitedCaves = Cave.visit(visitedCaves, startingCave)
  edgeCaves
  ->Js.Array2.map(edgeCave => {
    if Cave.isEndCave(edgeCave) {
      1
    } else if Cave.isVisited(visitedCaves, edgeCave) && Cave.isSmall(edgeCave) {
      0
    } else {
      getPathsCount(caves, edgeCave, visitedCaves)
    }
  })
  ->Js_array2.reduce((acc, cur) => acc + cur, 0)
}

let rec getPathsCount2 = (caves, startingCave, visitedCaves, visitedSmallCave) => {
  let edgeCaves = Cave.getEdgeCaves(caves, startingCave)
  let visitedCaves = Cave.visit(visitedCaves, startingCave)
  edgeCaves
  ->Js.Array2.map(edgeCave => {
    if Cave.isEndCave(edgeCave) {
      1
    } else if Cave.isVisited(visitedCaves, edgeCave) && Cave.isSmall(edgeCave) {
      switch visitedSmallCave {
      | true => 0
      | false => edgeCave.id === "start" ? 0 : getPathsCount2(caves, edgeCave, visitedCaves, true)
      }
    } else {
      getPathsCount2(caves, edgeCave, visitedCaves, visitedSmallCave)
    }
  })
  ->Js_array2.reduce((acc, cur) => acc + cur, 0)
}

let caves =
  Utils.getInputLines("src/input/Day12.txt")
  ->Js_array2.map(input => {
    let caves = input->Js.String2.split("-")
    (caves[0], caves[1])
  })
  ->Js_array2.reduce((caves, (start, end)) => {
    let caves = if Cave.doesExist(caves, start) {
      caves
    } else {
      Js_array2.concat(caves, [Cave.make(start)])
    }
    let caves = if Cave.doesExist(caves, end) {
      caves
    } else {
      Js_array2.concat(caves, [Cave.make(end)])
    }
    caves->Js.Array2.map(cave => {
      if cave.id === start {
        {...cave, paths: Js.Array2.concat(cave.paths, [end])}
      } else if cave.id === end {
        {...cave, paths: Js.Array2.concat(cave.paths, [start])}
      } else {
        cave
      }
    })
  }, [])

let startCave = caves->Js.Array2.find(cave => cave.id === "start")
switch startCave {
| Some(startCave) => {
    Js.log(getPathsCount(caves, startCave, []))
    Js.log(getPathsCount2(caves, startCave, [], false))
  }
| None => Js.log("no")
}
