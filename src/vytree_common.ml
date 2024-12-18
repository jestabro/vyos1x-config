
let name_of n = Vytree.name_of_node n
let children_of n = Vytree.children_of_node n

let find_child n c =
    Vytree.find n (name_of c)

let insert_child n c =
    Vytree.insert ~position:Vytree.Lexical ~children:(children_of c) n [(name_of c)] (Vytree.data_of_node c)

let replace_child n c =
    Vytree.replace n c

let (^~) node node' =
    (Vytree.name_of_node node) = (Vytree.name_of_node node') &&
    (Vytree.data_of_node node) <> (Vytree.data_of_node node')
