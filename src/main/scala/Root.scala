import scala.collection.mutable.Queue

object Root {
	def findNeighbors(adjMat: Array[Array[Int]], node: Int): List[Int] = 
		adjMat(node).zipWithIndex.filter(_._1 == 1).map(_._2).toList

	def toAdjList(adjMat: Array[Array[Int]]): Array[List[Int]] = 
		adjMat.map(x => x.zipWithIndex.filter(_._1 == 1).map(_._2).toList)

	def findRoot(adjMat: Array[Array[Int]]): Int = {
		val adjList = toAdjList(adjMat)
		def findFurthest(cur: Int, last: List[Int], dist: Int): (Int, List[Int],Int) = {
			val neighs = adjList(cur).filter(_!=last.head)
			if(neighs.isEmpty) (cur,dist)
			else {
				if(last.head == -1)
					neighs.map(findA(_,List(cur),dist+1)).sortWith(_._2> _._2).head
				else
					neighs.map(findA(_,cur :: last,dist+1)).sortWith(_._2> _._2).head
			}
		}
		val a = findFurthest(0,List(-1),0)
		val b = findFurthest(a._1,List(-1),0)
		val diameter = b._3 
		val median = diameter/2
		b._2(median)
	}
	def makeRootedMatrix(adjMat: Array[Array[Int]]): Array[Array[Int]] = {
		var root = findRoot(adjMat)
		var neighs = findNeighbors(adjMat, root)
		val out = adjMat.deep.copy()

		while(neighs.nonempty){
			for(neigh <- neighs)
				out(neigh)(root) = 0
			roots = roots.enqueue(neighs)
			root = roots.dequeue
			neighs = findNeighbors(root)
		}
		out
	}
	def rootCanonicalName(adjMat: Array[Array[Int]]): Array[String] = {
		val out = Array.fill(adjMat.size)("")
		val root = findRoot(adjMat)
		val rootedMat = makeRootedMatrix(adjMat)

		def assignCanonicalNames(node: Int): Unit = {
			if(rootedMat(node).count(_==1) == 0) out(node) = "10"
			else{
				val children = rootedMat(node).map(child => assignCanonicalNames(child)).sortWith(_.length < _.length)
				out(node) = "1 " + children.mkString(" ") + " 0"
			}
		}
		out(root)
	}

	def ahuTreeIsomorphism(t1: Array[Array[Int]], t2: Array[Array[Int]]): Boolean =
		rootCanonicalName(t1) == rootCanonicalName(t2)
}