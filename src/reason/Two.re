module CharMap = Map.Make(Char);

type boxId = string;

let inputLines = () =>
  Node.Fs.readFileAsUtf8Sync("resources/two.txt")
  |> Js.String.split("\n")
  |> Array.to_list;

let arrayFromString = (s: string): array(char) =>
  Array.init(String.length(s), String.get(s));

let characterHistogram = (arrayOfChars: array(char)) =>
  Array.fold_left(
    (map, aChar) =>
      switch (CharMap.find(aChar, map)) {
      | item => CharMap.add(aChar, item + 1, map)
      | exception Not_found => CharMap.add(aChar, 1, map)
      },
    CharMap.empty,
    arrayOfChars,
  );

let hasValue = (value: 'a, map: CharMap.t('a)): bool =>
  CharMap.exists((_, binding) => binding == value, map);

let boxIdHistogram = (boxId: boxId) =>
  boxId |> arrayFromString |> characterHistogram;

let checksum = (boxIds: array(boxId)): int => {
  let histograms = Array.map(boxIdHistogram, boxIds);

  let numberOfTwos =
    Array.fold_left(
      (total, histogram) => hasValue(2, histogram) ? total + 1 : total,
      0,
      histograms,
    );

  let numberOfThrees =
    Array.fold_left(
      (total, histogram) => hasValue(3, histogram) ? total + 1 : total,
      0,
      histograms,
    );

  numberOfThrees * numberOfTwos;
};

let firstSolution = () => inputLines() |> Array.of_list |> checksum;
