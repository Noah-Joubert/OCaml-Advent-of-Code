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
let listOfString (stringInput: string) : char list =
  let rec aux (stringRemaining : string) =
    match stringRemaining with 
    | "" -> []
    | x -> 
      let stringRemaining' = String.sub stringRemaining 1 (String.length stringRemaining - 1) in
      let c = String.get x 0 in
      c :: aux stringRemaining'
  in aux stringInput;;


let rec findMessageStart (message: char list) (n: int) : int =
  let rec exist elem lst =
    match lst with
    | [] -> false
    | hd::tl -> elem = hd || exist elem tl
  in let rec dupExist lst =
    match lst with
    | [] -> false
    | hd::tl -> (exist hd tl) || dupExist tl

  in let rec getNextThree (remaining: char list) (count : int): char list =
    if count > 3 then [] else 
      match remaining with 
      | [] -> []
      | h::t -> h :: (getNextThree t (count + 1))
  in 
  match message with 
  | [] -> n + 4
  | h::t -> if dupExist (h::(getNextThree t 1)) then findMessageStart t (n+1) else n + 4;;

let fileInput = read_file "6.txt";;
let fileInputAsList = listOfString (List.hd fileInput);;
let messageStart = findMessageStart fileInputAsList 0;;

(*-------------=+ Part 2 +=-------------*)
let rec findNewMessageStart (message: char list) (n: int) : int =
  let rec exist elem lst =
    match lst with
    | [] -> false
    | hd::tl -> elem = hd || exist elem tl
  in let rec dupExist lst =
    match lst with
    | [] -> false
    | hd::tl -> (exist hd tl) || dupExist tl

  in let rec getThirteen (remaining: char list) (count : int): char list =
    if count > 13 then [] else 
      match remaining with 
      | [] -> []
      | h::t -> h :: (getThirteen t (count + 1))
  in 
  match message with 
  | [] -> n + 14
  | h::t -> if dupExist (h::(getThirteen t 1)) then findNewMessageStart t (n+1) else n + 14;;
let messageStart = findNewMessageStart fileInputAsList 0;;
