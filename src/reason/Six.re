type coordinate = (int, int);

module Coordinate = {
  type t = coordinate;
  let compare = Pervasives.compare;
};

module CoordinateMap = Map.Make(Coordinate);
module CoordinateSet = Set.Make(Coordinate);
module IntSet =
  Set.Make({
    type t = int;
    let compare = Pervasives.compare;
  });

let getX = ((x, _): coordinate) => x;
let getY = ((_, y): coordinate) => y;

let coordinates_of_string = (input: string) => {
  let matchAttempt = Js.String.match([%re "/(\d+), (\d+)/"], input);

  switch (matchAttempt) {
  | Some(matches) => (int_of_string(matches[1]), int_of_string(matches[2]))
  | None => raise(Not_found)
  };
};

let inputCoordinates = () =>
  Node.Fs.readFileAsUtf8Sync("resources/six.txt")
  |> Js.String.split("\n")
  |> Array.map(coordinates_of_string)
  |> Array.to_list;

let boundaryCoordinates = (coordinates: list(coordinate)) => {
  let sortedByY =
    List.sort(((_, a), (_, b)) => Pervasives.compare(a, b), coordinates);
  let sortedByX =
    List.sort(((a, _), (b, _)) => Pervasives.compare(a, b), coordinates);

  let (_, lowerY) = List.hd(sortedByY);
  let (_, upperY) = List.nth(sortedByY, List.length(sortedByY) - 1);

  let (lowerX, _) = List.hd(sortedByX);
  let (upperX, _) = List.nth(sortedByX, List.length(sortedByX) - 1);

  List.filter(
    ((x, y)) => x == lowerX || x == upperX || y == lowerY || y == upperY,
    coordinates,
  );
};

let rangeIn = (dimension, coordinates: list(coordinate)) => {
  let boundaries = boundaryCoordinates(coordinates);

  let sorted =
    List.sort(
      (a, b) => Pervasives.compare(dimension(a), dimension(b)),
      boundaries,
    );

  let min = dimension(List.hd(sorted));
  let max = dimension(List.nth(sorted, List.length(sorted) - 1));

  (min, max);
};

let distance = (a: coordinate, b: coordinate) => {
  let (ax, ay) = a;
  let (bx, by) = b;

  Pervasives.abs(ax - bx) + Pervasives.abs(ay - by);
};

let inspectList = l => {
  Js.log(Array.of_list(l));

  l;
};

let closestBy =
    (discriminator, origin: coordinate, coordinates: list(coordinate)) => {
  let neighbor =
    coordinates
    |> List.sort((a, b) => {
         let aComparison = discriminator(origin, a);
         let bComparison = discriminator(origin, b);

         Pervasives.compare(aComparison, bComparison);
       })
    |> List.hd;

  let distanceToNearestNeighbor = distance(origin, neighbor);

  List.filter(
    coordinate => distance(origin, coordinate) == distanceToNearestNeighbor,
    coordinates,
  );
};

let gridWithBoundaries = (minX, maxX, minY, maxY) => {
  let space = [||];

  for (x in minX to maxX) {
    for (y in minY to maxY) {
      let _ = Js.Array.push((x, y), space);
      ();
    };
  };

  space;
};

let firstAnswer = (coordinates: list(coordinate)) => {
  let boundaryCoordinates = boundaryCoordinates(coordinates);

  let (minX, maxX) = rangeIn(getX, coordinates);
  let (minY, maxY) = rangeIn(getY, coordinates);

  let space = gridWithBoundaries(minX, maxX, minY, maxY);

  let noSharedCoordinates = (areaA, areaB) => {
    let axs = IntSet.of_list(List.map(getX, areaA));
    let ays = IntSet.of_list(List.map(getY, areaA));

    let bxs = IntSet.of_list(List.map(getX, areaB));
    let bys = IntSet.of_list(List.map(getY, areaB));

    IntSet.cardinal(IntSet.inter(axs, bxs)) == 0
    && IntSet.cardinal(IntSet.inter(ays, bys)) == 0;
  };

  let coordinateAreas =
    Array.fold_left(
      (map, coordinate) => {
        let closest = closestBy(distance, coordinate, coordinates);

        switch (closest) {
        | [areaOwner] =>
          switch (CoordinateMap.find(areaOwner, map)) {
          | entry =>
            CoordinateMap.add(areaOwner, [coordinate, ...entry], map)
          | exception Not_found =>
            CoordinateMap.add(areaOwner, [coordinate], map)
          }
        | _ => map
        };
      },
      CoordinateMap.empty,
      space,
    );

  let (_origin, area) =
    coordinateAreas
    |> CoordinateMap.bindings
    |> List.filter(((coordinate, area)) =>
         !List.mem(coordinate, boundaryCoordinates)
         && noSharedCoordinates(area, boundaryCoordinates)
       )
    |> List.sort(((_, areaA), (_, areaB)) =>
         Pervasives.compare(List.length(areaB), List.length(areaA))
       )
    |> List.hd;

  List.length(area);
};

let totalDistance = (origin: coordinate, coordinates: list(coordinate)) =>
  List.fold_left(
    (total, coordinate) => total + distance(origin, coordinate),
    0,
    coordinates,
  );

let secondAnswer = (coordinates: list(coordinate), maxDistance: int) => {
  let (minX, maxX) = rangeIn(getX, coordinates);
  let (minY, maxY) = rangeIn(getY, coordinates);

  let space = gridWithBoundaries(minX, maxX, minY, maxY);

  let region =
    Array.fold_left(
      (set, gridLocation) =>
        if (totalDistance(gridLocation, coordinates) < maxDistance) {
          CoordinateSet.add(gridLocation, set);
        } else {
          set;
        },
      CoordinateSet.empty,
      space,
    );

  CoordinateSet.cardinal(region);
};

Js.log(secondAnswer(inputCoordinates(), 10000));
