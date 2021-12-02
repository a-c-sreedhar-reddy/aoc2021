let measurements =
  Node_fs.readFileSync("./src/Day01.txt", #utf8)->Js.String2.split("\n")
    |> Js.Array.map(Js.Float.fromString)

let part1 = () => {
  let largerMeasurements = measurements->Js.Array2.reducei((acc, current, index) => {
    let firstElement = index === 0
    if firstElement {
      acc
    } else if measurements->Js.Array2.unsafe_get(index - 1) <= current {
      acc + 1
    } else {
      acc
    }
  }, 0)
  largerMeasurements
}

let part2 = () => {
  let rec getLargeMeasurement = (measurements, start) => {
    if start + 3 >= measurements->Js.Array.length {
      0
    } else {
      let first_sum = measurements[start] +. measurements[start + 1] +. measurements[start + 2]
      let second_sum = measurements[start + 1] +. measurements[start + 2] +. measurements[start + 3]
      if first_sum < second_sum {
        1 + getLargeMeasurement(measurements, start + 1)
      } else {
        getLargeMeasurement(measurements, start + 1)
      }
    }
  }
  getLargeMeasurement(measurements, 0)
}

Js.log(part1())
Js.log(part2())
