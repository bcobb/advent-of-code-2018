let input = () => Node.Fs.readFileAsUtf8Sync("resources/five.txt");

let array_of_string = (source: string) =>
  Array.init(String.length(source), String.get(source));

let shouldReact = (a: char, b: char) =>
  Char.lowercase(a) == Char.lowercase(b) && a != b;

let reactOnce = (source: string) =>
  array_of_string(source)
  |> Array.fold_left(
       (result, character) => {
         let lastIndex = String.length(result) - 1;
         if (lastIndex >= 0) {
           let lastCharacter = result.[lastIndex];

           if (shouldReact(lastCharacter, character)) {
             String.sub(result, 0, lastIndex);
           } else {
             result ++ String.make(1, character);
           };
         } else {
           result ++ String.make(1, character);
         };
       },
       "",
     );

let react = (source: string) => {
  let origin = ref(source);
  let reaction = ref(reactOnce(source));

  while (reaction^ != origin^) {
    origin := reaction^;
    reaction := reactOnce(origin^);
  };

  reaction^;
};

let firstAnswer = () => Js.log(String.length(react(input())));

let secondAnswer = () => {
  let basePolymer = input();

  let unitsToTryRemoving = [
    [%re "/a/ig"],
    [%re "/b/ig"],
    [%re "/c/ig"],
    [%re "/d/ig"],
    [%re "/e/ig"],
    [%re "/f/ig"],
    [%re "/g/ig"],
    [%re "/h/ig"],
    [%re "/i/ig"],
    [%re "/j/ig"],
    [%re "/k/ig"],
    [%re "/l/ig"],
    [%re "/m/ig"],
    [%re "/n/ig"],
    [%re "/o/ig"],
    [%re "/p/ig"],
    [%re "/q/ig"],
    [%re "/r/ig"],
    [%re "/s/ig"],
    [%re "/t/ig"],
    [%re "/u/ig"],
    [%re "/v/ig"],
    [%re "/w/ig"],
    [%re "/x/ig"],
    [%re "/y/ig"],
    [%re "/z/ig"],
  ];

  unitsToTryRemoving
  |> List.map(re =>
       basePolymer |> Js.String.replaceByRe(re, "") |> react |> String.length
     )
  |> List.sort(Pervasives.compare)
  |> List.hd
  |> Js.log;
};
