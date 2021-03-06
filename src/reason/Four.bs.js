// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var $$Map = require("bs-platform/lib/js/map.js");
var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_primitive = require("bs-platform/lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function range(start, stop) {
  return $$Array.init(stop - start | 0, (function (i) {
                return start + i | 0;
              }));
}

function join(sep, list) {
  if (list) {
    var tail = list[1];
    var tail$1 = list[0];
    if (tail) {
      return tail$1 + (sep + join(sep, tail));
    } else {
      return tail$1;
    }
  } else {
    return "";
  }
}

function line_of_string(string) {
  var beginShift = string.match((/(\d{2})\] Guard #(\d+) begins/));
  if (beginShift !== null) {
    return /* record */[
            /* action : BeginShift */[Caml_array.caml_array_get(beginShift, 2)],
            /* minute */Caml_format.caml_int_of_string(Caml_array.caml_array_get(beginShift, 1))
          ];
  } else {
    var fallAsleep = string.match((/(\d{2})\] falls asleep/));
    if (fallAsleep !== null) {
      return /* record */[
              /* action : FallAsleep */0,
              /* minute */Caml_format.caml_int_of_string(Caml_array.caml_array_get(fallAsleep, 1))
            ];
    } else {
      var wakeUp = string.match((/(\d{2})\] wakes up/));
      if (wakeUp !== null) {
        return /* record */[
                /* action : WakeUp */1,
                /* minute */Caml_format.caml_int_of_string(Caml_array.caml_array_get(wakeUp, 1))
              ];
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    }
  }
}

function inputLines(param) {
  return List.map(line_of_string, List.sort(Caml_obj.caml_compare, $$Array.to_list(Fs.readFileSync("resources/four.txt", "utf8").split("\n"))));
}

var StringMap = $$Map.Make([$$String.compare]);

function guardIds(lines) {
  return List.fold_left((function (ids, line) {
                var match = line[/* action */0];
                if (typeof match === "number") {
                  return ids;
                } else {
                  return /* :: */[
                          match[0],
                          ids
                        ];
                }
              }), /* [] */0, lines);
}

function addActionToHead(actions, action) {
  var current_001 = List.hd(actions);
  var current = /* :: */[
    action,
    current_001
  ];
  var rest = List.tl(actions);
  return List.append(/* :: */[
              current,
              /* [] */0
            ], rest);
}

function guardActions(lines) {
  return List.fold_left((function (actions, line) {
                var match = line[/* action */0];
                if (typeof match === "number") {
                  return addActionToHead(actions, line);
                } else {
                  return /* :: */[
                          /* :: */[
                            line,
                            /* [] */0
                          ],
                          actions
                        ];
                }
              }), /* [] */0, lines);
}

function sleeps(lines) {
  return List.filter((function (line) {
                  return line[/* action */0] === /* FallAsleep */0;
                }))(lines);
}

function wakes(lines) {
  return List.filter((function (line) {
                  return line[/* action */0] === /* WakeUp */1;
                }))(lines);
}

function minutesAsleep(lines) {
  var sleepingLines = sleeps(lines);
  var wakingLines = wakes(lines);
  return $$Array.fold_left($$Array.append, /* array */[], $$Array.init(List.length(sleepingLines), (function (i) {
                    var start = List.nth(sleepingLines, i)[/* minute */1];
                    var stop = List.nth(wakingLines, i)[/* minute */1];
                    return range(start, stop);
                  })));
}

function shifts_of_lines(lines) {
  var ids = guardIds(lines);
  var actions = guardActions(lines);
  return $$Array.to_list($$Array.init(List.length(ids), (function (i) {
                    var guardId = List.nth(ids, i);
                    var entries = List.rev(List.nth(actions, i));
                    var minutesAsleep$1 = minutesAsleep(entries);
                    var timeAsleep = minutesAsleep$1.length;
                    return /* record */[
                            /* guardId */guardId,
                            /* minutesAsleep */minutesAsleep$1,
                            /* timeAsleep */timeAsleep
                          ];
                  })));
}

function sleepiestGuardId(lines) {
  return List.hd(List.sort((function (param, param$1) {
                      return Caml_primitive.caml_int_compare(param$1[1], param[1]);
                    }), Curry._1(StringMap[/* bindings */16], List.fold_left((function (map, shift) {
                              var exit = 0;
                              var entry;
                              try {
                                entry = Curry._2(StringMap[/* find */21], shift[/* guardId */0], map);
                                exit = 1;
                              }
                              catch (exn){
                                if (exn === Caml_builtin_exceptions.not_found) {
                                  return Curry._3(StringMap[/* add */3], shift[/* guardId */0], shift[/* timeAsleep */2], map);
                                } else {
                                  throw exn;
                                }
                              }
                              if (exit === 1) {
                                return Curry._3(StringMap[/* add */3], shift[/* guardId */0], entry + shift[/* timeAsleep */2] | 0, map);
                              }
                              
                            }), StringMap[/* empty */0], shifts_of_lines(lines)))))[0];
}

function sleepiestMinuteForGuard(guardId, lines) {
  return List.hd(List.sort((function (param, param$1) {
                      return Caml_primitive.caml_int_compare(param$1[1], param[1]);
                    }), Curry._1(StringMap[/* bindings */16], $$Array.fold_left((function (map, minute) {
                              var exit = 0;
                              var entry;
                              try {
                                entry = Curry._2(StringMap[/* find */21], minute, map);
                                exit = 1;
                              }
                              catch (exn){
                                if (exn === Caml_builtin_exceptions.not_found) {
                                  return Curry._3(StringMap[/* add */3], minute, 1, map);
                                } else {
                                  throw exn;
                                }
                              }
                              if (exit === 1) {
                                return Curry._3(StringMap[/* add */3], minute, entry + 1 | 0, map);
                              }
                              
                            }), StringMap[/* empty */0], $$Array.map((function (prim) {
                                  return String(prim);
                                }), List.fold_left($$Array.append, /* array */[], List.map((function (shift) {
                                          return shift[/* minutesAsleep */1];
                                        }), List.filter((function (shift) {
                                                return shift[/* guardId */0] === guardId;
                                              }))(shifts_of_lines(lines)))))))))[0];
}

function secondAnswer(param) {
  var match = List.hd(List.sort((function (param, param$1) {
              return Caml_primitive.caml_int_compare(param$1[2], param[2]);
            }), List.map((function (param) {
                  var minutesAsleep = param[1];
                  var guardId = param[0];
                  if (minutesAsleep.length !== 0) {
                    var match = List.hd(List.sort((function (param, param$1) {
                                return Caml_primitive.caml_int_compare(param$1[1], param[1]);
                              }), Curry._1(StringMap[/* bindings */16], $$Array.fold_left((function (map, minute) {
                                        var exit = 0;
                                        var entry;
                                        try {
                                          entry = Curry._2(StringMap[/* find */21], minute, map);
                                          exit = 1;
                                        }
                                        catch (exn){
                                          if (exn === Caml_builtin_exceptions.not_found) {
                                            return Curry._3(StringMap[/* add */3], minute, 1, map);
                                          } else {
                                            throw exn;
                                          }
                                        }
                                        if (exit === 1) {
                                          return Curry._3(StringMap[/* add */3], minute, entry + 1 | 0, map);
                                        }
                                        
                                      }), StringMap[/* empty */0], $$Array.map((function (prim) {
                                            return String(prim);
                                          }), minutesAsleep)))));
                    return /* tuple */[
                            guardId,
                            match[0],
                            match[1]
                          ];
                  } else {
                    return /* tuple */[
                            guardId,
                            "0",
                            0
                          ];
                  }
                }), Curry._1(StringMap[/* bindings */16], List.fold_left((function (map, shift) {
                          var exit = 0;
                          var entry;
                          try {
                            entry = Curry._2(StringMap[/* find */21], shift[/* guardId */0], map);
                            exit = 1;
                          }
                          catch (exn){
                            if (exn === Caml_builtin_exceptions.not_found) {
                              return Curry._3(StringMap[/* add */3], shift[/* guardId */0], shift[/* minutesAsleep */1], map);
                            } else {
                              throw exn;
                            }
                          }
                          if (exit === 1) {
                            return Curry._3(StringMap[/* add */3], shift[/* guardId */0], $$Array.append(entry, shift[/* minutesAsleep */1]), map);
                          }
                          
                        }), StringMap[/* empty */0], shifts_of_lines(inputLines(/* () */0)))))));
  var minute = match[1];
  var guardId = match[0];
  var answer = String(Caml_int32.imul(Caml_format.caml_int_of_string(guardId), Caml_format.caml_int_of_string(minute)));
  console.log("Guard #" + (guardId + (" spent minute " + (minute + (" asleep " + (String(match[2]) + (" times => " + answer)))))));
  return /* () */0;
}

function firstAnswer(param) {
  var lines = inputLines(/* () */0);
  var guardId = sleepiestGuardId(lines);
  var minute = sleepiestMinuteForGuard(guardId, lines);
  var answer = String(Caml_int32.imul(Caml_format.caml_int_of_string(guardId), Caml_format.caml_int_of_string(minute)));
  console.log(guardId + (" * " + (minute + (" = " + answer))));
  return /* () */0;
}

secondAnswer(/* () */0);

exports.range = range;
exports.join = join;
exports.line_of_string = line_of_string;
exports.inputLines = inputLines;
exports.StringMap = StringMap;
exports.guardIds = guardIds;
exports.addActionToHead = addActionToHead;
exports.guardActions = guardActions;
exports.sleeps = sleeps;
exports.wakes = wakes;
exports.minutesAsleep = minutesAsleep;
exports.shifts_of_lines = shifts_of_lines;
exports.sleepiestGuardId = sleepiestGuardId;
exports.sleepiestMinuteForGuard = sleepiestMinuteForGuard;
exports.secondAnswer = secondAnswer;
exports.firstAnswer = firstAnswer;
/* StringMap Not a pure module */
