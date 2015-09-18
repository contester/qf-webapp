# Submits and results

- Arrived(timestamp: DateTime, seconds: Int, afterFreeze: Boolean)
    * Ordering, scoring, filtering for pre/after freeze
- RatedProblem(id: String, rating: Int)
    * problem rating is used in scoring
- SubmitId(id: Int, arrived: Arrived, teamId: Int, contestId: Int, problem: RatedProblem, ext: String)
    * ext is only for information
- Submit(submitId: SubmitId, finished: Boolean, compiled: Boolean, passed: Int, taken: Int, testingId: Option\[Int\])
    * this is enough to do the scoring?
- ResultEntry(test: Int, result: Int, time: Int, memory: Long, info: Int, testerExitCode: Int, testerOutput: String, testerError: String)
    * 1:1 from Results
- FullyDescribedSubmit(submit: Submit, index: Int, score: Option\[Score\], result: SubmitResult, stats: SubmitStats, details: Seq\[ResultEntry\])
    * used for list of submits
- SubmitDetails(fsub: FullyDescribedSubmit, source: Array\[Byte\])
- SubmitResult
    * SubmitWaiting
    * SubmitAccepted
    * SubmitCompileError
    * SubmitPartialResult(passed: Int, taken: Int)
    * SubmitACMPartialResult(text: String, test: Option\[Int\])