module IntSet = Set.Make(Int32);

type operation =
  | Positive
  | Negative;
type amount = Int32.t;
type frequencyChange = (operation, amount);

let inputLines = () =>
  Node.Fs.readFileAsUtf8Sync("resources/one.txt")
  |> Js.String.split("\n")
  |> Array.to_list;

let lineToOperation = (line: string): operation =>
  line.[0] == '+' ? Positive : Negative;
let lineToAmount = (line: string): Int32.t => {
  let rangeEnd = String.length(line);

  String.sub(line, 1, rangeEnd - 1) |> Int32.of_string;
};

let lineToFrequencyChange = (line: string): frequencyChange => (
  lineToOperation(line),
  lineToAmount(line),
);

let applyFrequencyChange = (total: Int32.t, change: frequencyChange): Int32.t =>
  switch (change) {
  | (Positive, amount) => Int32.add(total, amount)
  | (Negative, amount) => Int32.sub(total, amount)
  };

let circularStream = (l: list('a)): Stream.t('a) => {
  let length = List.length(l);

  Stream.from(i => Some(List.nth(l, i mod length)));
};

let findFirstRepeatFrequency = l => {
  let changes = circularStream(l);
  let knownFrequencies = ref(IntSet.empty);
  let frequency = ref(Int32.zero);
  let break = ref(false);

  while (! break^) {
    frequency := applyFrequencyChange(frequency^, Stream.next(changes));

    if (IntSet.mem(frequency^, knownFrequencies^)) {
      break := true;
    } else {
      knownFrequencies := IntSet.add(frequency^, knownFrequencies^);
    };
  };

  frequency^;
};

let firstSolution = () =>
  inputLines()
  |> List.map(lineToFrequencyChange)
  |> List.fold_left(applyFrequencyChange, Int32.zero);

let secondSolution = () =>
  inputLines() |> List.map(lineToFrequencyChange) |> findFirstRepeatFrequency;
