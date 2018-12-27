// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var $$Map = require("bs-platform/lib/js/map.js");
var $$Set = require("bs-platform/lib/js/set.js");
var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

var compare = Caml_obj.caml_compare;

var Step = /* module */[/* compare */compare];

var StepMap = $$Map.Make(Step);

var StepSet = $$Set.Make(Step);

function instruction_of_string(input) {
  var matchAttempt = input.match((/Step ([A-Z]).+before step ([A-Z])/));
  if (matchAttempt !== null) {
    return /* record */[
            /* prerequisite */Caml_array.caml_array_get(matchAttempt, 1),
            /* subject */Caml_array.caml_array_get(matchAttempt, 2)
          ];
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
}

function inputInstructions(param) {
  return $$Array.to_list($$Array.map(instruction_of_string, Fs.readFileSync("resources/seven.txt", "utf8").split("\n")));
}

function prerequisitesByStep(instructions) {
  var emptyStepMap = List.fold_left((function (map, instruction) {
          return Curry._3(StepMap[/* add */3], instruction[/* subject */1], StepSet[/* empty */0], Curry._3(StepMap[/* add */3], instruction[/* prerequisite */0], StepSet[/* empty */0], map));
        }), StepMap[/* empty */0], instructions);
  return List.fold_left((function (map, instruction) {
                var exit = 0;
                var entry;
                try {
                  entry = Curry._2(StepMap[/* find */21], instruction[/* subject */1], map);
                  exit = 1;
                }
                catch (exn){
                  if (exn === Caml_builtin_exceptions.not_found) {
                    return Curry._3(StepMap[/* add */3], instruction[/* subject */1], Curry._2(StepSet[/* add */3], instruction[/* prerequisite */0], StepSet[/* empty */0]), map);
                  } else {
                    throw exn;
                  }
                }
                if (exit === 1) {
                  return Curry._3(StepMap[/* add */3], instruction[/* subject */1], Curry._2(StepSet[/* add */3], instruction[/* prerequisite */0], entry), map);
                }
                
              }), emptyStepMap, instructions);
}

function firstStep(instructions) {
  return List.hd(Curry._1(StepMap[/* bindings */16], Curry._2(StepMap[/* filter */13], (function (_step, prerequisites) {
                          return Curry._1(StepSet[/* is_empty */1], prerequisites);
                        }), prerequisitesByStep(instructions))))[0];
}

function inspectList(l) {
  console.log($$Array.of_list(l));
  return l;
}

function completionOrder(_prerequisitesByStep, _completedSteps) {
  while(true) {
    var completedSteps = _completedSteps;
    var prerequisitesByStep = _prerequisitesByStep;
    if (Curry._1(StepMap[/* is_empty */1], prerequisitesByStep)) {
      return List.rev(completedSteps);
    } else {
      var completedStepsSet = Curry._1(StepSet[/* of_list */25], completedSteps);
      var nextAvailableStep = List.hd(List.sort(Caml_obj.caml_compare, List.map((function (param) {
                      return param[0];
                    }), Curry._1(StepMap[/* bindings */16], Curry._2(StepMap[/* filter */13], (function(completedStepsSet){
                          return function (_step, prerequisites) {
                            return Curry._2(StepSet[/* subset */11], prerequisites, completedStepsSet);
                          }
                          }(completedStepsSet)), prerequisitesByStep)))));
      _completedSteps = /* :: */[
        nextAvailableStep,
        completedSteps
      ];
      _prerequisitesByStep = Curry._2(StepMap[/* remove */5], nextAvailableStep, prerequisitesByStep);
      continue ;
    }
  };
}

function firstAnswer(param) {
  var instructions = inputInstructions(/* () */0);
  return List.fold_left((function (answer, step) {
                return answer + step;
              }), "", completionOrder(prerequisitesByStep(instructions), /* [] */0));
}

console.log(firstAnswer(/* () */0));

exports.Step = Step;
exports.StepMap = StepMap;
exports.StepSet = StepSet;
exports.instruction_of_string = instruction_of_string;
exports.inputInstructions = inputInstructions;
exports.prerequisitesByStep = prerequisitesByStep;
exports.firstStep = firstStep;
exports.inspectList = inspectList;
exports.completionOrder = completionOrder;
exports.firstAnswer = firstAnswer;
/* StepMap Not a pure module */