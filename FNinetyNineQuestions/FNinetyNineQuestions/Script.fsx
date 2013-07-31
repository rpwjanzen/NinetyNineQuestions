﻿// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
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
    