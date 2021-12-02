type instruction = Forward(int) | Down(int) | Up(int)
let instructions =
  Node_fs.readFileSync("./src/Day02.txt", #utf8)
  ->Js.String2.split("\n")
  ->Js.Array2.map(instruction => {
    switch instruction->Js.String2.split(" ") {
    | ["forward", x] => Forward(x->Belt.Int.fromString->Belt.Option.getWithDefault(0))
    | ["down", x] => Down(x->Belt.Int.fromString->Belt.Option.getWithDefault(0))
    | ["up", x] => Up(x->Belt.Int.fromString->Belt.Option.getWithDefault(0))
    | _ => raise(Not_found)
    }
  })

//part 1
let (horizontal, vertical) = instructions->Js.Array2.reduce((acc, currentInstruction) => {
  let (horizontal, vertical) = acc
  switch currentInstruction {
  | Forward(x) => (horizontal + x, vertical)
  | Down(x) => (horizontal, vertical + x)
  | Up(x) => (horizontal, vertical - x)
  }
}, (0, 0))
Js.log(horizontal * vertical)

//part 2
let (horizontal, vertical, _) = instructions->Js.Array2.reduce((acc, currentInstruction) => {
  let (horizontal, vertical, aim) = acc
  switch currentInstruction {
  | Forward(x) => (horizontal + x, vertical + aim * x, aim)
  | Down(x) => (horizontal, vertical, aim + x)
  | Up(x) => (horizontal, vertical, aim - x)
  }
}, (0, 0, 0))
Js.log(horizontal * vertical)
