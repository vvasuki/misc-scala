package vvasuki.util

object system {
  private val runtime = Runtime.getRuntime()
  import runtime.{ totalMemory, freeMemory, maxMemory }
  def getMemoryStatistic = {
    "used " + (totalMemory - freeMemory) + " free " + freeMemory

  }

}