import scala.collection.mutable.Queue

object Root {

	def findRoot(adjMat: Array[Array[Int]]): Int = {
		val adjList = adjMat.map(x => x.zipWithIndex.filter(_._1 == 1).map(_._2).toList)

		def findFurthest(cur: Int, last: List[Int], dist: Int): (Int, List[Int],Int) {
			val neighs = adjList(cur).filter(_!=last.head)
			if(neighs.isEmpty) (cur,dist)
			else {
				last.head match {
					case -1 => return neighs.map(findA(_,List(cur),dist+1)).sortWith(_._2> _._2).head
					case _ => return neighs.map(findA(_,cur :: last,dist+1)).sortWith(_._2> _._2).head
				}
			}
		}

		val a = findFurthest(0,List(-1),0)
		val b = findFurthest(a._1,List(-1),0)
		val diameter = b._3 
		val median = diameter/2
		b._2(median)
	}
}