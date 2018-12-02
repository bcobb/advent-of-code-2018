module CharMap = Map.Make(Char);

type boxId = string;
type boxIdCombo = (boxId, boxId);

let inputLines = () =>
  Node.Fs.readFileAsUtf8Sync("resources/two.txt")
  |> Js.String.split("\n")
  |> Array.to_list;

let stringAsCollection = (s: string): array(char) =>
  Array.init(String.length(s), String.get(s));

let characterHistogram = (chars: array(char)) =>
  Array.fold_left(
    (map, aChar) =>
      switch (CharMap.find(aChar, map)) {
      | item => CharMap.add(aChar, item + 1, map)
      | exception Not_found => CharMap.add(aChar, 1, map)
      },
    CharMap.empty,
    chars,
  );

let hasValue = (value: 'a, map: CharMap.t('a)): bool =>
  CharMap.exists((_, binding) => binding == value, map);

let boxIdHistogram = (boxId: boxId) =>
  boxId |> stringAsCollection |> characterHistogram;

let checksum = (boxIds: list(boxId)): int => {
  let histograms = List.map(boxIdHistogram, boxIds);

  let numberOfTwos =
    List.fold_left(
      (total, histogram) => hasValue(2, histogram) ? total + 1 : total,
      0,
      histograms,
    );

  let numberOfThrees =
    List.fold_left(
      (total, histogram) => hasValue(3, histogram) ? total + 1 : total,
      0,
      histograms,
    );

  numberOfThrees * numberOfTwos;
};

let positionalEqualities = (combo: boxIdCombo) => {
  let (boxIdA, boxIdB) = combo;

  Array.mapi(
    (index, char) => boxIdB.[index] == char,
    stringAsCollection(boxIdA),
  );
};

let offByOne = (combo: boxIdCombo) =>
  1
  == Array.fold_left(
       (inequalities, isEqual) => isEqual ? inequalities : inequalities + 1,
       0,
       positionalEqualities(combo),
     );

let allCombinationsOfTwo = (boxIds: list(boxId)): list(boxIdCombo) =>
  List.map(
    boxId => List.map(otherBoxId => (boxId, otherBoxId), boxIds),
    boxIds,
  )
  |> List.flatten;

let commonLetters = (combo: boxIdCombo) => {
  let (boxIdA, boxIdB) = combo;

  stringAsCollection(boxIdA)
  |> Array.mapi((index, char) =>
       boxIdB.[index] == char ? String.make(1, char) : ""
     )
  |> Array.to_list
  |> String.concat("");
};

let firstSolution = () => inputLines() |> checksum;

let secondSolution = () =>
  inputLines() |> allCombinationsOfTwo |> List.find(offByOne) |> commonLetters;
