module Point = {
  type t = {x: int, y: int}
  let make = (x, y) => {x: x, y: y}
}

module Line = {
  type alignment = Horizontal | Vertical | Slant
  type t = {start: Point.t, end: Point.t, alignment: alignment}

  let make = ((x1, y1), (x2, y2)) => {
    let start = Point.make(x1, y1)
    let end = Point.make(x2, y2)
    if start.x === end.x {
      if start.y > end.y {
        {start: end, end: start, alignment: Vertical}
      } else {
        {start: start, end: end, alignment: Vertical}
      }
    } else if start.y === end.y {
      if start.x > end.x {
        {start: end, end: start, alignment: Horizontal}
      } else {
        {start: start, end: end, alignment: Horizontal}
      }
    } else if start.x > end.x {
      {start: end, end: start, alignment: Slant}
    } else {
      {start: start, end: end, alignment: Slant}
    }
  }
}

module Board = {
  let make = () => Belt_Array.make(1000, [])->Belt_Array.map(_ => Belt_Array.make(1000, 0))
  let count = board => board->Js.Array2.reduce((acc, row) => {
      acc + row->Js.Array2.filter(element => element >= 2)->Js.Array2.length
    }, 0)
}

let inputLines =
  Utils.getInputLines("src/input/Day05.txt")
  ->Js.Array2.map(line =>
    line
    ->Js.String2.split(" -> ")
    ->Js.Array2.map(pointString => {
      let point = pointString->Js.String2.split(",")
      (
        point[0]->Belt_Int.fromString->Belt_Option.getWithDefault(0),
        point[1]->Belt_Int.fromString->Belt_Option.getWithDefault(0),
      )
    })
  )
  ->Js.Array2.map(points => Line.make(points[0], points[1]))
let board = Board.make()

inputLines->Js.Array2.forEach(line => {
  switch line.alignment {
  | Line.Horizontal =>
    for i in line.start.x to line.end.x {
      board[i][line.start.y] = board[i][line.start.y] + 1
    }
  | Line.Vertical =>
    for i in line.start.y to line.end.y {
      board[line.start.x][i] = board[line.start.x][i] + 1
    }
  | Line.Slant => ()
  }
})
Js.log(board->Board.count)

let board = Board.make()
inputLines->Js.Array2.forEach(line => {
  switch line.alignment {
  | Line.Horizontal =>
    for i in line.start.x to line.end.x {
      board[i][line.start.y] = board[i][line.start.y] + 1
    }
  | Line.Vertical =>
    for i in line.start.y to line.end.y {
      board[line.start.x][i] = board[line.start.x][i] + 1
    }
  | Line.Slant =>
    if line.end.y > line.start.y {
      for i in line.start.x to line.end.x {
        let y = i - line.start.x + line.start.y
        board[i][y] = board[i][y] + 1
      }
    } else {
      for i in line.start.x to line.end.x {
        let y = line.start.y - (i - line.start.x)
        board[i][y] = board[i][y] + 1
      }
    }
  }
})

Js.log(board->Board.count)
