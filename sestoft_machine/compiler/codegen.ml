open Compiler
open Instructions
open Environment

let incrementInstructionCount (count:int) (instr:instruction) : int =
  match instr with
  | Take          -> count+1
  | Enter _       -> count+2
  | Push _        -> count+2
  | Sep           -> count+1
  | Let _         -> count+2
  | Enterglobal _ -> count+2
  | Pushglobal _  -> count+2
;;

let instructionToCode (instruction:instruction) (labelEnv:env) : int list =
  match instruction with
  | Take -> [0]
  | Enter n -> [1; n]
  | Push n -> [2; n]
  | Sep -> [-3]
  | Let i -> [4; i]
  | Enterglobal s -> [5; (lookup labelEnv s)]
  | Pushglobal s -> [6; (lookup labelEnv s)]
;;

let rec generateScEnv (compiledScs:compiledSc list) (instructionCount:int) : env =
  match compiledScs with
  | [] -> []
  | (name, args, body)::rest -> 
      (name, instructionCount) :: 
      (generateScEnv rest ((List.fold_left incrementInstructionCount instructionCount body)))
;;

let rec codeGenerationHelper (compiledScs:compiledSc list) (scEnv:env) =
  match compiledScs with
  | [] -> []
  | (name, args, body)::rest ->
      List.flatten (List.map (fun inst -> instructionToCode inst scEnv) body) @
      codeGenerationHelper rest scEnv
;;

let codeGeneration (compiledScs:compiledSc list) : int list =
  let env = generateScEnv compiledScs 2 in
  (instructionToCode (Enterglobal "main")) env @ codeGenerationHelper compiledScs env
;;
