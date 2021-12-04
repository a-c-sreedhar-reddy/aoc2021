module Board = {
  type element = Marked(int) | UnMarked(int)
  type status = Bingo | NotYetBingo
  type t = array<array<element>>
  let duplicateBoard = (board: t) => {
    board->Js.Array2.map(row => row->Js.Array2.map(a => a))
  }

  let findUnMarkedNumberPosition = (board: t, number) => {
    board->Js.Array2.reducei((acc, row, rowIndex) => {
      switch acc {
      | None => {
          let index = row->Belt_Array.getIndexBy(element =>
            switch element {
            | UnMarked(i) => i === number
            | Marked(_) => false
            }
          )
          switch index {
          | Some(col) => Some(rowIndex, col)
          | None => acc
          }
        }
      | Some(_) => acc
      }
    }, None)
  }

  let getColumn = (board, index) => {
    board->Js.Array2.map(row => {
      row[index]
    })
  }

  let allElementsAreMarked = row => {
    row->Js.Array2.every(element =>
      switch element {
      | Marked(_) => true
      | UnMarked(_) => false
      }
    )
  }
  let allRowElementsAreMarked = (board, row) => {
    allElementsAreMarked(board[row])
  }
  let allColumnElementsAreMarked = (board, column) => {
    allElementsAreMarked(getColumn(board, column))
  }
  let drawNumber = (board, number) => {
    let numberPosition = findUnMarkedNumberPosition(board, number)
    switch numberPosition {
    | Some(row, col) =>
      board[row][col] = Marked(number)
      if board->allRowElementsAreMarked(row) || board->allColumnElementsAreMarked(col) {
        Bingo
      } else {
        NotYetBingo
      }
    | None => NotYetBingo
    }
  }
  let sumOfunMarkedElements = board => {
    board->Js.Array2.reduce((acc, row) => acc + row->Js.Array2.reduce((acc, element) =>
        switch element {
        | UnMarked(i) => acc + i
        | Marked(_) => acc
        }
      , 0), 0)
  }
}

module Input = {
  let inputLines = Utils.getInputLines("src/input/Day04.txt")
  let numbers =
    inputLines[0]
    ->Js.String2.split(",")
    ->Js.Array2.map(i => Belt_Int.fromString(i)->Belt_Option.getWithDefault(0))

  let boards = []
  let numberOfBoards = (inputLines->Js.Array2.length - 1) / 6
  let parseRow = str => {
    str
    ->Js.String2.split(" ")
    ->Js.Array2.filter(a => a != "")
    ->Belt_Array.map(i => Board.UnMarked(Belt_Int.fromString(i)->Belt_Option.getWithDefault(0)))
  }
  for i in 1 to numberOfBoards {
    let start = 2 + (i - 1) * 6
    let board: Board.t =
      [
        inputLines[start],
        inputLines[start + 1],
        inputLines[start + 2],
        inputLines[start + 3],
        inputLines[start + 4],
      ]->Js.Array2.map(parseRow)
    boards->Js.Array2.push(board)->ignore
  }
}
let boards = Input.boards
let numbers = Input.numbers
let boardsForPart2 = boards->Js.Array2.map(board => Board.duplicateBoard(board))

let part1 = numbers->Js.Array2.reduce((acc, number) => {
  switch acc {
  | Some(_) => acc
  | None =>
    let bingoedBoardIndex =
      boards
      ->Js.Array2.map(board => {
        board->Board.drawNumber(number)
      })
      ->Belt_Array.getIndexBy(status => {
        switch status {
        | Board.Bingo => true
        | Board.NotYetBingo => false
        }
      })

    switch bingoedBoardIndex {
    | None => acc
    | Some(boardIndex) => {
        let unmarkedSum = Board.sumOfunMarkedElements(boards[boardIndex])
        Some(unmarkedSum * number)
      }
    }
  }
}, None)

let (_, part2) = numbers->Js.Array2.reduce((acc, number) => {
  let (boards, solution) = acc
  if !(solution->Belt.Option.isNone) {
    acc
  } else {
    let boardsStatus = boards->Js.Array2.map(board => {
      board->Board.drawNumber(number)
    })

    switch boardsStatus {
    | [Board.Bingo] => {
        let unmarkedSum = Board.sumOfunMarkedElements(boards[0])

        ([], Some(unmarkedSum * number))
      }
    | _ => (boards->Js.Array2.filteri((_, i) => boardsStatus[i] != Board.Bingo), None)
    }
  }
}, (boardsForPart2, None))

Js.log(part1)
Js.log(part2)
