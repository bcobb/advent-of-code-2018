type step = string;
type instruction = {
  prerequisite: step,
  subject: step,
};

module Step = {
  type t = step;
  let compare = Pervasives.compare;
};

module StepMap = Map.Make(Step);
module StepSet = Set.Make(Step);

let instruction_of_string = (input: string) => {
  let matchAttempt =
    Js.String.match([%re "/Step ([A-Z]).+before step ([A-Z])/"], input);

  switch (matchAttempt) {
  | Some(matches) => {prerequisite: matches[1], subject: matches[2]}
  | None => raise(Not_found)
  };
};

let inputInstructions = () =>
  Node.Fs.readFileAsUtf8Sync("resources/seven.txt")
  |> Js.String.split("\n")
  |> Array.map(instruction_of_string)
  |> Array.to_list;

let prerequisitesByStep = (instructions: list(instruction)) => {
  let emptyStepMap =
    List.fold_left(
      (map, instruction) =>
        map
        |> StepMap.add(instruction.prerequisite, StepSet.empty)
        |> StepMap.add(instruction.subject, StepSet.empty),
      StepMap.empty,
      instructions,
    );

  List.fold_left(
    (map, instruction) =>
      switch (StepMap.find(instruction.subject, map)) {
      | entry =>
        StepMap.add(
          instruction.subject,
          StepSet.add(instruction.prerequisite, entry),
          map,
        )
      | exception Not_found =>
        StepMap.add(
          instruction.subject,
          StepSet.add(instruction.prerequisite, StepSet.empty),
          map,
        )
      },
    emptyStepMap,
    instructions,
  );
};

let firstStep = (instructions: list(instruction)) => {
  let (step, _) =
    instructions
    |> prerequisitesByStep
    |> StepMap.filter((_step, prerequisites) =>
         StepSet.is_empty(prerequisites)
       )
    |> StepMap.bindings
    |> List.hd;

  step;
};

let inspectList = l => {
  Js.log(Array.of_list(l));

  l;
};

let rec completionOrder = (prerequisitesByStep, completedSteps) =>
  if (StepMap.is_empty(prerequisitesByStep)) {
    List.rev(completedSteps);
  } else {
    let completedStepsSet = StepSet.of_list(completedSteps);

    let nextAvailableStep =
      prerequisitesByStep
      |> StepMap.filter((_step, prerequisites) =>
           StepSet.subset(prerequisites, completedStepsSet)
         )
      |> StepMap.bindings
      |> List.map(((step, _prerequisites)) => step)
      |> List.sort(Pervasives.compare)
      |> List.hd;

    completionOrder(
      StepMap.remove(nextAvailableStep, prerequisitesByStep),
      [nextAvailableStep, ...completedSteps],
    );
  };

let firstAnswer = () => {
  let instructions = inputInstructions();

  completionOrder(prerequisitesByStep(instructions), [])
  |> List.fold_left((answer, step) => answer ++ step, "");
};

Js.log(firstAnswer());
