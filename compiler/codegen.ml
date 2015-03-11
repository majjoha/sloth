open Compiler
open Instructions

let incrementInstructionCount (count:int) (instr:instruction) : int = 
    match instr with
    | Pushglobal _   -> count+2
    | Push _         -> count+2
    | Pushint _      -> count+2
    | Mkap           -> count+1
    | Unwind         -> count+1
    | Slide _        -> count+2
    | Jump _         -> count+2
    | Update _       -> count+2
    | Pop _          -> count+2
    | Alloc _        -> count+2
;;

let rec generateLabelEnv (compiledScs : compiledSc list) (instructionCount:int) : (string * int) list =
  match compiledScs with
  | [] -> []
  | (name, args, body)::rest -> 
      (name, instructionCount) :: 
      (generateLabelEnv rest ((List.fold_left incrementInstructionCount instructionCount body)+1))
;;

let instructionToCode (instruction:instruction) (labelEnv:(string * int) list) : int list = 
  match instruction with
  | Pushglobal f -> [0; (lookup labelEnv f)]
  | Push n -> [1; n]
  | Pushint i -> [2; i]
  | Mkap -> [3]
  | Unwind -> [4]
  | Slide i -> [5; i]
  | Jump f -> [6; (lookup labelEnv f)]
  | Update n -> [7; n]
  | Pop i -> [8; i]
  | Alloc n -> [9; n]
;;

let rec codeGenerationHelper (compiledScs : compiledSc list) (labelEnv : (string * int) list) =
  match compiledScs with
  | [] -> []
  | (name, args, body)::rest -> 
      args :: List.flatten (List.map (fun inst -> instructionToCode inst labelEnv) body) @ 
      codeGenerationHelper rest labelEnv
;;

let codeGeneration (compiledScs : compiledSc list) : int list =
  let labelEnv = generateLabelEnv compiledScs 
                 (incrementInstructionCount (incrementInstructionCount 0 (Pushglobal "main")) Unwind) in
  (instructionToCode (Pushglobal "main") labelEnv) @ (instructionToCode Unwind labelEnv)
  @ codeGenerationHelper compiledScs labelEnv
;;
