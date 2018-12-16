type action =
  | BeginShift(string)
  | FallAsleep
  | WakeUp;

type line = {
  action,
  minute: int,
};

let range = (start: int, stop: int) =>
  Array.init(stop - start, i => start + i);

let rec join = (sep: string, list: list(string)) =>
  switch (list) {
  | [] => ""
  | [tail] => tail
  | [head, ...tail] => head ++ sep ++ join(sep, tail)
  };

let line_of_string = (string: string) => {
  let beginShift =
    Js.String.match([%re "/(\d{2})\] Guard #(\d+) begins/"], string);

  switch (beginShift) {
  | Some(guardIdMatches) => {
      action: BeginShift(guardIdMatches[2]),
      minute: int_of_string(guardIdMatches[1]),
    }
  | None =>
    let fallAsleep =
      Js.String.match([%re "/(\d{2})\] falls asleep/"], string);

    switch (fallAsleep) {
    | Some(fallAsleepMatches) => {
        action: FallAsleep,
        minute: int_of_string(fallAsleepMatches[1]),
      }
    | None =>
      let wakeUp = Js.String.match([%re "/(\d{2})\] wakes up/"], string);

      switch (wakeUp) {
      | Some(wakeUpMatches) => {
          action: WakeUp,
          minute: int_of_string(wakeUpMatches[1]),
        }
      | None => raise(Not_found)
      };
    };
  };
};

let inputLines = () =>
  Node.Fs.readFileAsUtf8Sync("resources/four.txt")
  |> Js.String.split("\n")
  |> Array.to_list
  |> List.sort(Pervasives.compare)
  |> List.map(line_of_string);

type shift = {
  guardId: string,
  minutesAsleep: array(int),
  timeAsleep: int,
};

module StringMap = Map.Make(String);

let guardIds = (lines: list(line)) =>
  lines
  |> List.fold_left(
       (ids, line) =>
         switch (line.action) {
         | BeginShift(id) => [id, ...ids]
         | _ => ids
         },
       [],
     );

let addActionToHead = (actions: list(list(line)), action: line) => {
  let current = [action, ...List.hd(actions)];
  let rest = List.tl(actions);

  List.append([current], rest);
};

let guardActions = (lines: list(line)) =>
  lines
  |> List.fold_left(
       (actions, line) =>
         switch (line.action) {
         | BeginShift(_) => [[line], ...actions]
         | WakeUp => addActionToHead(actions, line)
         | FallAsleep => addActionToHead(actions, line)
         },
       [],
     );

let sleeps = (lines: list(line)) =>
  List.filter(line => line.action == FallAsleep, lines);

let wakes = (lines: list(line)) =>
  List.filter(line => line.action == WakeUp, lines);

let minutesAsleep = (lines: list(line)) => {
  let sleepingLines = sleeps(lines);
  let wakingLines = wakes(lines);

  Array.init(
    List.length(sleepingLines),
    i => {
      let start = List.nth(sleepingLines, i).minute;
      let stop = List.nth(wakingLines, i).minute;

      range(start, stop);
    },
  )
  |> Array.fold_left(Array.append, [||]);
};

let shifts_of_lines = (lines: list(line)) => {
  let ids = guardIds(lines);
  let actions = guardActions(lines);

  Array.init(
    List.length(ids),
    i => {
      let guardId = List.nth(ids, i);
      let entries = List.rev(List.nth(actions, i));
      let minutesAsleep = minutesAsleep(entries);
      let timeAsleep = Array.length(minutesAsleep);

      {guardId, minutesAsleep, timeAsleep};
    },
  )
  |> Array.to_list;
};

let sleepiestGuardId = (lines: list(line)) => {
  let (guardId, _) =
    shifts_of_lines(lines)
    |> List.fold_left(
         (map, shift) =>
           switch (StringMap.find(shift.guardId, map)) {
           | entry =>
             StringMap.add(shift.guardId, entry + shift.timeAsleep, map)
           | exception Not_found =>
             StringMap.add(shift.guardId, shift.timeAsleep, map)
           },
         StringMap.empty,
       )
    |> StringMap.bindings
    |> List.sort(((_, a), (_, b)) => Pervasives.compare(b, a))
    |> List.hd;

  guardId;
};

let sleepiestMinuteForGuard = (guardId, lines: list(line)) => {
  let (minute, _) =
    shifts_of_lines(lines)
    |> List.filter(shift => shift.guardId == guardId)
    |> List.map(shift => shift.minutesAsleep)
    |> List.fold_left(Array.append, [||])
    |> Array.map(string_of_int)
    |> Array.fold_left(
         (map, minute) =>
           switch (StringMap.find(minute, map)) {
           | entry => StringMap.add(minute, entry + 1, map)
           | exception Not_found => StringMap.add(minute, 1, map)
           },
         StringMap.empty,
       )
    |> StringMap.bindings
    |> List.sort(((_, a), (_, b)) => Pervasives.compare(b, a))
    |> List.hd;

  minute;
};

let firstAnswer = () => {
  let lines = inputLines();

  let guardId = sleepiestGuardId(lines);
  let minute = sleepiestMinuteForGuard(guardId, lines);
  let answer =
    string_of_int(int_of_string(guardId) * int_of_string(minute));

  Js.log(guardId ++ " * " ++ minute ++ " = " ++ answer);
};
