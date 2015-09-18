

- Arrived(timestamp: DateTime, seconds: Int, afterFreeze: Boolean)
- RatedProblem(id: String, rating: Int)
- SubmitId(id: Int, arrived: Arrived, teamId: Int, contestId: Int, problem: RatedProblem, ext: String)
- SubmitTopLevel(finished: Boolean, compiled: Boolean, passed: Int, taken: Int)
- Submit(submitId: SubmitId, status: SubmitTopLevel, testingId: Option\[Int\])
- ResultEntry(test: Int, result: Int, time: Int, memory: Long, info: Int, testerExitCode: Int, testerOutput: String, testerError: String)
- FullyDescribedSubmit(submit: Submit, index: Int, score: Option\[Score\], result: SubmitResult, stats: SubmitStats, details: Seq\[ResultEntry\])
- SubmitDetails(fsub: FullyDescribedSubmit, source: Array\[Byte\])