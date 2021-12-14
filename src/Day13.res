module Paper = {
  type element = Hash | Dot
  type t = array<array<element>>
  let width = paper => {
    switch Belt_Array.get(paper, 0) {
    | Some(row) => row->Js_array2.length
    | None => 0
    }
  }
  let height = paper => {
    paper->Js.Array2.length
  }
  let make = (~width, ~height, ~defaultElement=Dot, ()): t => {
    Belt.Array.makeBy(height, _ => Belt_Array.make(width, defaultElement))
  }
  let row = (paper, row) => {
    if paper->Js_array2.length <= row || row < 0 {
      None
    } else {
      Some(paper[row])
    }
  }
  let column = (paper, column) => {
    switch paper->Belt_Array.length {
    | 0 => None
    | _ =>
      switch paper[0]->Js.Array2.length {
      | 0 => None
      | len if column >= len || column < 0 => None
      | _ =>
        Some(
          paper->Js_array2.map(row => {
            row[column]
          }),
        )
      }
    }
  }
  let mergeTwoRows = (row1: array<element>, row2: array<element>): array<element> => {
    Belt_Array.mapWithIndex(row1, (index, element1) => {
      let element2 = row2[index]
      switch (element1, element2) {
      | (Hash, Dot) => Hash
      | (Dot, Hash) | (Hash, Hash) => Hash
      | (Dot, Dot) => Dot
      }
    })
  }
  let updateColumn = (paper, index, column) => {
    switch Belt_Array.get(paper, 0) {
    | None => ()
    | Some(row) if index > row->Js.Array2.length => ()
    | _ => paper->Js.Array2.forEachi((row, rowIndex) => row[index] = column[rowIndex])
    }
  }
  let updateRow = (paper, index, row) => {
    switch Belt_Array.get(paper, index) {
    | None => ()
    | _ => paper[index] = row
    }
  }
  let markPoints = (paper, points) => {
    points->Js_array2.forEach(((x, y)) => paper[y][x] = Hash)
    paper
  }
  let hashCount = paper => {
    paper
    ->Js.Array2.map(row => row->Js.Array2.filter(element => element === Hash)->Js.Array2.length)
    ->Js.Array2.reduce((acc, cur) => acc + cur, 0)
  }
}

let foldLeft = (paper, ~xaxis) => {
  let newWidth = Js.Math.max_int(xaxis, Paper.width(paper) - xaxis - 1)
  let newPaper = Paper.make(~width=newWidth, ~height=Paper.height(paper), ())

  for i in newWidth downto 1 {
    let col1 = Paper.column(paper, xaxis + i)
    let col2 = Paper.column(paper, xaxis - i)
    let mergedColumn = switch (col1, col2) {
    | (None, None) => None
    | (Some(col), None) => Some(col)
    | (None, Some(col)) => Some(col)
    | (Some(col1), Some(col2)) => Some(Paper.mergeTwoRows(col1, col2))
    }
    switch mergedColumn {
    | None => ()
    | Some(column) => Paper.updateColumn(newPaper, newWidth - i, column)
    }
  }
  newPaper
}
let foldTop = (paper, ~yaxis) => {
  let newHeight = Js.Math.max_int(yaxis, Paper.height(paper) - yaxis - 1)
  let newPaper = Paper.make(~height=newHeight, ~width=Paper.width(paper), ())

  for i in newHeight downto 1 {
    let col1 = Paper.row(paper, yaxis + i)
    let col2 = Paper.row(paper, yaxis - i)
    let mergedColumn = switch (col1, col2) {
    | (None, None) => None
    | (Some(col), None) => Some(col)
    | (None, Some(col)) => Some(col)
    | (Some(col1), Some(col2)) => Some(Paper.mergeTwoRows(col1, col2))
    }
    switch mergedColumn {
    | None => ()
    | Some(column) => Paper.updateRow(newPaper, newHeight - i, column)
    }
  }
  newPaper
}

let (points, folds, _): (Js_array2.t<(int, int)>, Js.Array2.t<([#x | #y], int)>, _) =
  Utils.getInputLines("src/input/Day13.txt")->Js_array2.reduce(((points, folds, mode), current) => {
    switch current {
    | "" => (points, folds, #Folds)
    | line if mode === #Points => {
        let point =
          Js.String2.split(line, ",")->Belt_Array.map(el =>
            el->Belt_Int.fromString->Belt_Option.getWithDefault(0)
          )
        let _ = Js_array2.push(points, (point[0], point[1]))
        (points, folds, mode)
      }
    | line => {
        let fold = Js.String2.split(line, " ")[2]->Js.String2.split("=")
        let axis = fold[0] === "x" ? #x : #y
        let _ =
          folds->Js.Array2.push((axis, fold[1]->Belt_Int.fromString->Belt_Option.getWithDefault(0)))
        (points, folds, mode)
      }
    }
  }, ([], [], #Points))
let width = points->Js_array2.map(((x, _)) => x)->Js.Math.maxMany_int
let height = points->Js_array2.map(((_, y)) => y)->Js.Math.maxMany_int
let paper = Paper.make(~width=width + 1, ~height=height + 1, ())->Paper.markPoints(points)

switch folds[0] {
| (#x, xaxis) => Js.log(paper->foldLeft(~xaxis)->Paper.hashCount)
| (#y, yaxis) => Js.log(paper->foldTop(~yaxis)->Paper.hashCount)
}

Js.log(
  folds
  ->Js.Array2.reduce((paper, fold) => {
    switch fold {
    | (#x, xaxis) => paper->foldLeft(~xaxis)
    | (#y, yaxis) => paper->foldTop(~yaxis)
    }
  }, paper)
  ->Js.Array2.map(row =>
    row
    ->Js.Array2.map(element =>
      switch element {
      | Paper.Dot => " "
      | Paper.Hash => "#"
      }
    )
    ->Js.Array2.joinWith(" ")
  ),
)
