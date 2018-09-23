package vvasuki.ranking

import scala.Array.canBuildFrom
import scala.collection.mutable.Buffer
import scala.io.Source
import com.weiglewilczek.slf4s.Logging
import java.io.FileOutputStream
import java.io.PrintStream

class Query(val query: Double, val items: Buffer[Double], val clickRate: Buffer[Double] = null) {
  def normalize(arr: Buffer[Double]) = {val max = arr.max; arr.map(_/max)}
  var scores = normalize(clickRate)
  def updateScores(rankClickRate: Buffer[Double], urlPopularities: Map[Double, Double]) = {
	  // clickRate relative to the average for the corresponding rank.
  	// ie a measure of how much (or little) people clicked an item irrespective of its low (or high) rank 
    var performanceWrtAvg = normalize((clickRate zip rankClickRate).map(x => x._1 / x._2))

    // Ideally this weight should be learned by training on data.
    val weightOnClickRate = 0.9
    // set scores(i) = wt * clickRate(i) + (1-wt) * performanceWrtAvg(i)
    scores = (scores.map(_ * weightOnClickRate) zip performanceWrtAvg.map(_ * (1-weightOnClickRate)))
    		.map(x => x._1 + x._2)
    		
    // Where scores are zero, use popularity in such a way that 
    //the popularity never causes an item to be ranked higher than an item with non-zero click rate.  
    val scoresByPopularity = items.map(urlPopularities(_))
    val nonZeroScores = scores.filter(_ != 0)
    val baseScore = if (nonZeroScores.isEmpty) 0.01 else nonZeroScores.min
    scores = (scores zip scoresByPopularity).map({
      case (s1, s2) =>
        if (s1 == 0) s2 * baseScore
        else s1
    })
  }

  def getScore(itemId: Double) = {
    scores(items.indexOf(itemId))
  }

}

object Rerank extends Logging {
  val dataDir = "/home/vvasuki/Dropbox/twitData/"

  def getData = {
    val urlPopularities = Source.fromFile(dataDir + "Popularity.csv").getLines.map(line => {
      val Array(queryId, popularity) = line.split(',').map(_.toDouble)
      (queryId, popularity / 100.0)
    }).toMap

    val rankClickRate = Source.fromFile(dataDir + "RankAvgCtr.csv").getLines.map(_.split(',').last.toDouble).toBuffer

    //QueryID, URLID, DisplayedRank, Impressions, Clicks
    class QueryItemRankLine(val queryId: Double, val itemId: Double, val rank: Double, val impressions: Double, val clicks: Double)

    val queryItemPerformanceLines = Source.fromFile(dataDir + "QueryItemRank.csv").getLines.map(line => {
      val Array(queryId, itemId, rank, impressions, clicks) = line.split(',').map(_.toDouble)
      new QueryItemRankLine(queryId, itemId, rank, impressions, clicks)
    }).toBuffer[QueryItemRankLine].groupBy(x => x.queryId).toMap

    val queryItemPerformance = queryItemPerformanceLines.mapValues(lines => {
      val sortedLines = lines.sortBy(_.rank)
      val items = sortedLines.map(_.itemId)
      val clickRate = sortedLines.map(x => x.clicks / x.impressions)
      val query = new Query(lines.head.queryId, items, clickRate)
      	query.updateScores(rankClickRate, urlPopularities)
      	query
    })
    queryItemPerformance
  }

  def saveResults(queryItemPerformance: Map[Double, Query]) = {
    val resultLines = Source.fromFile(dataDir + "Test.csv").getLines.map(line => {
      val fields = line.split(',').map(_.toDouble)
      val queryId = fields.head
      val items = fields.tail.toBuffer
      val scores = items.map(queryItemPerformance(queryId).getScore)
      (items zip scores).sortBy(-_._2).map(x => List(queryId, x._1, x._2).mkString(","))
    }).flatten
    val output = new PrintStream(new FileOutputStream(dataDir + "output.csv"))
    resultLines.foreach(output.println)
    output.close

  }
  def main(args: Array[String]) = {
    saveResults(getData)

  }
}