type distance = int;

type claim = {
  number: int,
  fromLeft: distance,
  fromTop: distance,
  width: distance,
  height: distance,
};

type point = (int, int);

module Point = {
  type t = point;
  let compare = Pervasives.compare;
};

module PointMap = Map.Make(Point);

let inputLines = () =>
  Node.Fs.readFileAsUtf8Sync("resources/three.txt")
  |> Js.String.split("\n")
  |> Array.to_list;

let nullClaim = {number: 0, fromTop: 0, fromLeft: 0, width: 0, height: 0};

let claim_of_string = (string: string): claim => {
  let result =
    Js.String.match([%re "/^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$/"], string);

  switch (result) {
  | None => nullClaim
  | Some(matches) => {
      number: int_of_string(matches[1]),
      fromLeft: int_of_string(matches[2]),
      fromTop: int_of_string(matches[3]),
      width: int_of_string(matches[4]),
      height: int_of_string(matches[5]),
    }
  };
};

let points_of_claim = (claim: claim): list(point) => {
  let leftStart = claim.fromLeft + 1;
  let leftEnd = leftStart + claim.width - 1;
  let topStart = claim.fromTop + 1;
  let topEnd = topStart + claim.height - 1;

  let points = ref([]);

  for (x in leftStart to leftEnd) {
    for (y in topStart to topEnd) {
      points := [(x, y), ...points^];
    };
  };

  points^;
};

let point_histogram = (list: list(point)) =>
  List.fold_left(
    (map, point) =>
      switch (PointMap.find(point, map)) {
      | entry => PointMap.add(point, entry + 1, map)
      | exception Not_found => PointMap.add(point, 1, map)
      },
    PointMap.empty,
    list,
  );

let first_solution = () =>
  inputLines()
  |> List.map(claim_of_string)
  |> List.map(points_of_claim)
  |> List.flatten
  |> point_histogram
  |> PointMap.filter((_, frequency) => frequency > 1)
  |> PointMap.cardinal;

let second_solution = () => {
  let claims = inputLines() |> List.map(claim_of_string);

  let solitary_points =
    claims
    |> List.map(points_of_claim)
    |> List.flatten
    |> point_histogram
    |> PointMap.filter((_, frequency) => frequency == 1);

  let solitary_claim =
    claims
    |> List.find(claim =>
         points_of_claim(claim)
         |> List.fold_left(
              (every, point) =>
                every && PointMap.mem(point, solitary_points),
              true,
            )
       );

  solitary_claim.number;
};
