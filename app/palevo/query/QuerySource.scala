package palevo.query

/** Where the query API reads contest data from. Placeholder: the methods are added with the endpoints. */
trait QuerySource

/** A source with no data, used until the real one is wired. */
object NoSource extends QuerySource
