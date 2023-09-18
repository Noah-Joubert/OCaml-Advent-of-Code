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

let toASCII (character: char) : int = 
  let asciiValue = Char.code character in
  if asciiValue >= 97 then asciiValue - 96
  else asciiValue - 64 + 26;;

let listOfString (stringInput: string) : char list =
  let rec aux (s : string) =
    match s with 
    | "" -> []
    | x -> (String.get s 0) :: aux (String.sub x 1 (String.length x - 1));

  in aux stringInput;;

let findDuplicatesInHalves (nums: int list) : int = 
  let rec exists num secondHalf l = 
    match l with 
    | [] -> false
    | h::t -> 
      if ((h = num) && (secondHalf >= 0)) then true 
      else exists num (secondHalf + 1) t 

  in let rec aux l halfWay = 
    match l with 
    | [] -> -1 (*Error!*)
    | h::t -> 
      if exists h (1-halfWay) t then h 
      else aux t (halfWay - 1)
  in aux nums (List.length nums / 2);;

let fileInput = read_file "3.txt";;
let fileInputInLists = List.map listOfString fileInput;;
let backpacksInAscii = (
  let rec aux l = 
    match l with 
    | [] -> []
    | h::t -> (List.map toASCII h) :: (aux t)
  in aux fileInputInLists
);;
let duplicates = List.map findDuplicatesInHalves backpacksInAscii;;
let sumOfDuplicates = List.fold_left (+) 0 duplicates ;;

(*-------------=+ Part 2 +=-------------*)
let rec groupedInThree (backPacks : 'a list list) : 'a list list list = 
  match backPacks with 
  | [] -> []
  | a::b::c::t -> [a;b;c] :: groupedInThree t
  | _ -> [];;

let findDuplicates (list1: 'a list) (list2: 'a list) : 'a list =
  let rec exists (toFind : 'a) (l : 'a list) : bool =
    match l with 
    | [] -> false
    | h::t -> if (h=toFind) then true else exists toFind t
    
  in let rec aux (list1: 'a list) (list2: 'a list) : 'a list = 
    match list1 with 
    | [] -> []
    | h::t -> if exists h list2 then h::(aux t list2) else aux t list2
  in aux list1 list2;;

let elementsInCommon (lists : 'a list list) : 'a list = 
  let rec aux (commonElements : 'a list) (l : 'a list list) = 
    match l with
    | [] -> commonElements
    | h::t -> aux (findDuplicates h commonElements) t
  in aux (List.hd lists) lists;;

let groups = groupedInThree backpacksInAscii;;
findDuplicates [1;2;3;4;5] [4;5;6;7;8];;
List.fold_left (+) 0 (List.map List.hd (List.map elementsInCommon groups));;