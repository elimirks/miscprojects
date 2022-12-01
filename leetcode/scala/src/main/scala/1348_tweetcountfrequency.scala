import scala.collection.mutable

// https://leetcode.com/problems/tweet-counts-per-frequency/
/**
 * Your TweetCounts object will be instantiated and called as such:
 * var obj = new TweetCounts()
 * obj.recordTweet(tweetName,time)
 * var param_2 = obj.getTweetCountsPerFrequency(freq,tweetName,startTime,endTime)
 */
class TweetCounts() {
  private val tweets: mutable.Map[String, List[Int]] = mutable.Map()

  /**
    * Stores the tweetName at the recorded time (in seconds)
    */
  def recordTweet(tweetName: String, time: Int) {
    // O(1) recording time is nice
    // Since the process would likely run offline, this is probably actually
    // better than storing directly in a BST.
    // Even _if_ we did use a BST here, getTweetCountsPerFrequency would still be O(n)
    tweets.put(tweetName, time +: tweets.getOrElse(tweetName, List()))
  }

  /**
    * Returns a list of integers representing the number of tweets with
    * tweetName in each time chunk for the given period of
    * time [startTime, endTime] (in seconds) and frequency freq
    *
    * O(n) solution overall. Reasonable runtime actually
    */
  def getTweetCountsPerFrequency(
    freq: String,
    tweetName: String,
    startTime: Int,
    endTime: Int
  ): List[Int] = {
    val inRange = tweets
      .getOrElse(tweetName, List())
      .filter(time => {
        // O(n), not the best. It would be better to use a BST to find the range
        // Or at least, store a sorted list so we don't have to iterate everything
        time >= startTime && time <= endTime
      })

    val chunkSize = freq match {
      case "minute" => 60
      case "hour"   => 3600
      case _        => 86400 // fallback to day sized chunks
    }

    val chunkCount = 1 + (endTime - startTime) / chunkSize

    val buckets = mutable.ArrayBuffer.fill[Int](chunkCount)(0)

    // O(n)
    inRange
      .map(_ - startTime)
      .foreach(time => {
        buckets(time / chunkSize) = buckets(time / chunkSize) + 1
      })

    buckets.toList
  }
}
