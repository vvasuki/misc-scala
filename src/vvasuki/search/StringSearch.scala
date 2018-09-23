import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import com.weiglewilczek.slf4s.Logging
import vvasuki.util.io.TextTableParser
import vvasuki.util.io.fileUtil
object StringSearch extends Logging {

  def cleanWord(word: String) = word.toUpperCase.replaceAll("\\W", "")

  // Match each word in the snippet with query words and return the result.
  def matchAndGetIndices(docWords: List[String], queryWords: List[String]) = {
    val bMatch = docWords.map(cleanWord).map(queryWords.contains)
    val matchingIndices = bMatch.indices.filter(x => bMatch(x))
    (bMatch, matchingIndices)
  }

  // How well does the snippet match the query words. Extra points if the query words occur consequtively.
  def getMatchingScore(docWords: List[String], queryWords: List[String]) = {
    val (bMatch, matchingIndices) = matchAndGetIndices(docWords, queryWords)
    if (matchingIndices.length == 0)
      0
    else {
      val numMatching = matchingIndices.length
      val numConsequtive = bMatch.sliding(2).map(x => x.forall(_ == true)).filter(x => x).length
      (numMatching + numConsequtive)
    }
  }

  // Given a snippet, how "centered" is it. Are the highlighted words close to the middle?
  def getCenterednessScore(docWords: List[String], queryWords: List[String]) = {
    val (bMatch, matchingIndices) = matchAndGetIndices(docWords, queryWords)
    val midPointIndex = (docWords.length.toDouble + 1) / 2
    val deviations = matchingIndices.map(_ + 1 - midPointIndex)
    deviations.map(x => x * x).sum / matchingIndices.length
  }

  // Given a snippet, highlight query words inside it.
  def highlight(docWords: List[String], queryWords: List[String]) = {
    val (bMatch, matchingIndices) = matchAndGetIndices(docWords, queryWords)
    val beginMarkup = "<HIGHLIGHT>"
    val endMarkup = "</HIGHLIGHT>"
    docWords.indices.map(i => if (!bMatch(i)) List(docWords(i))
    else {
      var lstWords = List(docWords(i))
      if (i == 0 || !bMatch(i - 1))
        lstWords = lstWords.+:(beginMarkup)
      if (i == docWords.length - 1 || !bMatch(i + 1))
        lstWords = lstWords.:+(endMarkup)
      lstWords
    }).flatten
  }

  // Find the best snippet and highlight query words therein.
  def highlight_doc(doc: String, query: String, slidingWindowLength: Int = 30) = {
    var textOut = ""
    val queryWords = query.split(' ').map(cleanWord).toList
    val docWords = doc.replaceAll("\\s+", " ").replaceAll("-", " ").split(' ').toList

    // A sliding window generates possible snippets
    val possibleSnippets = docWords.sliding(slidingWindowLength).toIndexedSeq
    // Get a match-score for each snippet.
    val matchScores = possibleSnippets.map(getMatchingScore(_, queryWords)).toIndexedSeq
    val filteredSnippets1 = (possibleSnippets zip matchScores).filter({ case (snippet, score) => score == matchScores.max }).map(x => x._1)
    if (filteredSnippets1.length > 0) {
      // Pick the most centered among the top-matching snippets.
      val centerednessScores = filteredSnippets1.map(getCenterednessScore(_, queryWords))
      val bestSnippetIndex = centerednessScores.findIndexOf(_ == centerednessScores.min)
      val bestSnippet = highlight(filteredSnippets1(bestSnippetIndex), queryWords)
      textOut = bestSnippet.mkString(" ")
    }

    textOut
  }

  def test = {
    val query = "deep dish pizza"
    var doc = "I like fish. Little star's deep dish pizza sure is fantastic. Dogs are funny."
    println(highlight_doc(doc, query))

    doc = """If you like deep dish pizza covered with marinara sauce you have to try this place out!

We had to wait for a hour to get a able but you can preorder your pizza so your not waiting for another 45 minutes after you sit down.

Btw: I read great reviews on the garlic bread and had to try it myself but beware its more like DIY garlic bread! They give you French bread/ butter/ cloves of soft garlic! It was good but next time I think I will just go for a extra slice of pizza!"""

    println(highlight_doc(doc, query))

    doc = """I know it's just pizza, but Little Star has a special place in our family's heart. When we came back from our honeymoon in Asia and needed a cheese fix, my wife and I went to Little Star for our first meal back in SF. After our baby was born, I called Little Star and brought it back to the hospital (they're very convenient to Kaiser).

My personal favorite is The Classic, which is the deep dish pizza with sausage, peppers, and mushrooms. When I'm rolling with the wife, who's vegetarian, we usually pick up a Little Star (spinach, mushrooms, ricotta, feta) or just a straight-up Cheese.

These are all deep-dish, mind you. I've tried the thin crust, which is fine, but the deep dish is where Little Star really shines. That cornmeal crust is just phenomenally good, and although the first melty slice usually gets the fork and knife treatment, I can usually hand-hold the rest of them - that's how strong (and yet delicious) the crust is.

I also highly recommend their mixed salad to start. It's a delicious mix of greens, cherry tomatoes, onions, walnuts, blue cheese, and vinaigrette. It's going to knock your socks off and prepare your belly for the main event."""

    println(highlight_doc(doc, query))
  }

  def main(args: Array[String]) = {
    test
  }
}
