namespace FsMal

module Types =
    // TODO: add support for functions, native functions, macros and hashmaps
    type Form =
        | Nil
        | Number of number: float
        | String of str: string
        | Bool of bool: bool
        | List of list: Form list
        | Vector of vec: Form array
        | Symbol of sym: string
        | Keyword of keyword: string
        | Atom of ref: Form ref
        | HashMap of
            keywordMap: Map<string, Form> *
            stringMap: Map<string, Form>
        | Skip
// Skip is a special type which is used to mark the parts of code which is not
// evaluated. It can not be used from the LISP userspace
