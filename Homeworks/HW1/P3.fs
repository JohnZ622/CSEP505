module P3

let rec first_answer f a_lst=
    match a_lst with
        [] -> None
        | head::tail ->
            match f head with
                None -> first_answer f tail
                | Some a -> Some a
