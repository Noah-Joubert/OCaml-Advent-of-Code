(*-------------=+ Part 1 +=-------------*)
let read_file (filename : string) : string list = 
  let lines = ref [] in
  let channel = open_in filename in
  
  try
    while true; do
      lines := (input_line channel)::!lines
    done; !lines
  with End_of_file ->
    close_in channel;
    List.rev !lines ;;


let evaluateGame (game : string) : int = 
  let p2 = String.get game 0 in 
  let p1 = String.get game 2 in
    (
      if p1 = 'X' then
        1 + 
          if (p2 = 'A') then 3 else 
          if (p2 = 'B') then 0 else 6
      else 0
     ) + (
      if p1 = 'Y' then
        2 + 
          if (p2 = 'B') then 3 else 
          if (p2 = 'C') then 0 else 6
      else 0
     ) + (
      if p1 = 'Z' then
        3 + 
          if (p2 = 'C') then 3 else 
          if (p2 = 'A') then 0 else 6
      else 0
     )

let games = read_file "2.txt";;
let scores = List.map evaluateGame games ;;
let totalScore = List.fold_left (+) 0 scores;;

(*-------------=+ Part 2 +=-------------*)
let findNextMove (game : string) : char = 
  let firstMove = String.get game 0 in 
  let outcome = String.get game 2 in
    if outcome = 'X' then 
      if firstMove = 'A' then 'Z'
      else if firstMove = 'B' then 'X'
      else 'Y'
    else if outcome = 'Y' then 
      if firstMove = 'A' then 'X'
      else if firstMove = 'B' then 'Y'
      else 'Z'
    else 
      if firstMove = 'A' then 'Y'
      else if firstMove = 'B' then 'Z'
      else 'X'

let reWriteGameString (game: string) : string = 
  Char.escaped (String.get game 0) ^ " " ^ Char.escaped(findNextMove game)

let newGameStrings = List.map reWriteGameString games;;
let newScores = List.map evaluateGame newGameStrings;;
let newTotalScore = List.fold_left (+) 0 newScores;;