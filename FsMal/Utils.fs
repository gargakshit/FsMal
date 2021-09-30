namespace FsMal

module Utils =
    open FParsec

    let betweenL
        (popen: Parser<_, _>)
        (pclose: Parser<_, _>)
        (p: Parser<_, _>)
        label
        =
        let expectedLabel = expected label

        let notClosedError (pos: Position) =
            messageError
                $"The %s{label} opened at %s{pos.ToString()} was not closed."

        fun (stream: CharStream<_>) ->
            // The following code might look a bit complicated, but that's mainly
            // because we manually apply three parsers in sequence and have to merge
            // the errors when they refer to the same parser state.
            let state0 = stream.State
            let reply1 = popen stream

            if reply1.Status = Ok then
                let stateTag1 = stream.StateTag
                let reply2 = p stream

                let error2 =
                    if stateTag1 <> stream.StateTag then
                        reply2.Error
                    else
                        mergeErrors reply1.Error reply2.Error

                if reply2.Status = Ok then
                    let stateTag2 = stream.StateTag
                    let reply3 = pclose stream

                    let error3 =
                        if stateTag2 <> stream.StateTag then
                            reply3.Error
                        else
                            mergeErrors error2 reply3.Error

                    if reply3.Status = Ok then
                        Reply(Ok, reply2.Result, error3)
                    else
                        Reply(
                            reply3.Status,
                            mergeErrors
                                error3
                                (notClosedError (state0.GetPosition(stream)))
                        )
                else
                    Reply(reply2.Status, reply2.Error)
            else
                let error =
                    if state0.Tag <> stream.StateTag then
                        reply1.Error
                    else
                        expectedLabel

                Reply(reply1.Status, error)
