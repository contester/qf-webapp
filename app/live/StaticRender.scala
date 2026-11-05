package live

import org.stingray.contester.dbmodel.Contest

/** Renders a static monitor page from the freezing board. */
object StaticRender {
  def render(contest: Contest, board: LiveBoard)(implicit messages: play.api.i18n.Messages): String =
    throw new NotImplementedError("replaced in Task 10")
}
