namespace FsMal

module Error =
    exception EvalException of string

    let symbolNotFound s = EvalException $"%s{s} not found"
