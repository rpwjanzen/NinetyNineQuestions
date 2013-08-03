// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open FNinetyNineQuestions

// From http://www.haskell.org/haskellwiki/99_questions/1_to_10

/// 1. Find the last element of a list.
let rec lastElement (ss:'a list) : ('a) =
    match ss with
    | [s] -> s
    | [] -> failwith "List must not be empty" 
    | x::xs -> lastElement xs

/// 2. Find the last but one element of a list.
let rec secondLastElement (ss:'a list) : ('a) =
    match ss with
    | [s;_] -> s
    | [_] -> failwith "List must have more than one element" 
    | [] -> failwith "List must not be empty" 
    | x::xs -> secondLastElement xs

/// 3. Find the K'th element of a list. The first element in the list is number 1.
let rec elementAt (n:int) (ss: 'a list) : ('a) =
    match ss with
    | _ when n = 0 -> failwith "Index must be at least 1"
    | [] -> failwith "Not enough elements"
    | x::xs when n = 1 -> x
    | x::xs -> elementAt (n - 1) xs

/// 4. Find the number of elements of a list.
let length (ss: 'a list) : (int) =
    let rec innerLength (ss: 'a list) (n:int) : (int) =
        match ss with
        | [] -> n
        | x::xs -> innerLength xs (n + 1)
    innerLength ss 0

/// 5. Reverse a list.
let reverse (ss: 'a list) : ('a list) =
    let rec innerReverse (ss: 'a list) (acc: 'a list) =
        match ss with
        | [] -> acc
        | x::xs -> innerReverse xs (List.Cons(x, acc))
    innerReverse ss []

/// 6. Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
let isPalindrome (ss:'a list) =
    let reversed = reverse ss
    let rec isSame (ss: 'a list) (qs: 'a list) =
        match (ss,qs) with
        | [],[] -> true
        | (x::xs),(y::ys) when x = y -> isSame xs ys
        | _ -> false
    isSame ss reversed

/// 7. Flatten a nested list structure.
/// 7. Flatten a nested list structure. (First attempt)
let flatten (ss: 'a list list) : ('a list) =
    let rec innerFlatten (ss: 'a list list) (acc:'a list) =
        match ss, acc with
        | [], acc -> acc
        | x::xs,acc -> innerFlatten xs (List.append acc x)
    innerFlatten ss []

/// 8. Eliminate consecutive duplicates of list elements.
let removeConsecutiveDuplicates (ss: 'a list) : ('a list) =
    let rec removeDuplicates (ss: 'a list) (acc: 'a list) : ('a list) =
        match ss with
        | x::y::xs when x = y ->
            removeDuplicates (y::xs) acc
        | x::y::xs when not(x = y) ->
            removeDuplicates (y::xs) (x::acc)
        | [x] ->
            List.rev (x::acc)
        | [] -> acc
        | _ -> failwith "Impossible?"
    match ss with
    | [] -> []
    | [a] -> [a]
    | xs -> removeDuplicates xs []

/// 9. Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
let pack (ss: 'a list) : ('a list list) =
    let isDuplicate (a:'a) (b:'a) = a = b
    let rec innerPack (ss: 'a list) (cs: 'a list) (acc: 'a list list) : ('a list list) =
        match ss,cs with
        | x::xs,y::_ when isDuplicate x y -> innerPack (xs) (x::cs) acc
        | x::xs,y::_ when not(isDuplicate x y) -> innerPack (xs) [x] (cs::acc)
        | [],cs -> List.rev (cs::acc)
        | _,_ -> failwith "Impossible?"
    match ss with
    | [] -> []
    | (x::xs) -> innerPack xs [x] []
    
/// 10. Run-length encoding of a list. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
let rle (ss: 'a list) : ((int * 'a) list) =
    pack ss |> List.map (fun (x::xs) -> (List.length (x::xs), x))

// 11. Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
type RleElement<'a> =
    | Single of 'a
    | Multiple of int * 'a

let rle2 (ss: 'a list) : (RleElement<'a> list) =
    let t = (fun (l, a) -> if (l = 1) then (Single a) else (Multiple (l, a)))
    (rle ss) |> List.map t

/// 12. Decode a run-length encoded list.
let decode (rs:RleElement<'a> list) : ('a list) =
    rs |> List.collect (fun r -> match r with Single v -> [v] | Multiple (l,a) -> List.init l (fun _ -> a))

/// 13. Run-length encoding of a list (direct solution). 
/// Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates,
/// as in problem 9, but only count them.
/// As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
let rle3 (ss: 'a list) : (RleElement<'a> list) =
    let isDuplicate (a:'a) (b:'a) = a = b
    let rec innerPack (ss: 'a list) (a:'a) (count:int) (acc: RleElement<'a> list) : (RleElement<'a> list) =
        match ss with
        | x::xs when isDuplicate x a -> innerPack (xs) a (count + 1) acc
        | x::xs when not(isDuplicate x a) -> innerPack (xs) x 1 ((if count = 1 then (Single a) else (Multiple (count, a))) ::acc)
        | [] -> List.rev ((Multiple (count,a))::acc)
        | _ -> failwith "Impossible?"
    match ss with
    | [] -> []
    | (x::xs) -> innerPack xs x 1 []

/// 14. Duplicate the elements of a list. e.g. [1;2;3] -> [1;1;2;2;3;3]
let duplicate (ss: 'a list) =
    ss |> List.collect (fun x -> [x;x])

/// 15. Replicate the elements of a list a given number of times.
let replicate (ss: 'a list) (n:int) =
    let rec innerReplicate (a:'a) (count:int) (acc:'a list) : ('a list) =
        if count = 0 then acc else innerReplicate a (count - 1) (a::acc)
    ss |> List.collect (fun x -> innerReplicate x n [])

/// 16. Drop every N'th element from a list.
let dropNth (ss: 'a list) (n:int) =
    let rec innerDropNth (ss: 'a list) (n:int) (count:int) (acc: 'a list) =
        match ss with
        | [] -> List.rev acc
        | (x::xs) -> if n = count then innerDropNth xs n 1 acc else innerDropNth xs n (count + 1) (x::acc)
    innerDropNth ss n 1 []

/// 17. Split a list into two parts; the length of the first part is given.
let split (ss: 'a list) (at:int) =
    let rec innerSplit (ss: 'a list) (c:int) (acc: 'a list) : ('a list * 'a list) =
        match ss with
        | (x::xs) -> if c = at then (acc |> List.rev, (x::xs)) else innerSplit xs (c + 1) (x::acc)
        | [] -> if c <= at then (acc |> List.rev, []) else ([], acc |> List.rev)
    innerSplit ss 0 []

/// 18. Extract a slice from a list. 
/// Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list
/// (both limits included). Start counting the elements with 1.
let slice (ss: 'a list) (s:int) (e:int) : ('a list) =
    ss |> List.toSeq |> Seq.skip (s - 1) |> Seq.take (e - s) |> Seq.toList
        
/// 19. Rotate a list N places to the left.
let rotate (ss: 'a list) (n:int) : ('a list) =
    let p0 = ss |> List.toSeq |> Seq.skip n |> Seq.toList
    let p1 = ss |> List.toSeq |> Seq.take n |> Seq.toList
    List.concat [p0; p1]

/// 20. Remove the K'th element from a list.
let removeAt (ss: 'a list) (n:int) : ('a list) =
    let p0 = ss |> List.toSeq |> Seq.skip (n + 1) |> Seq.toList
    let p1 = ss |> List.toSeq |> Seq.take n |> Seq.toList
    List.concat [p1; p0]

/// 21. Insert an element at a given position into a list.
let insertAt (ss: 'a list) (n:int) : ('a list) =
    