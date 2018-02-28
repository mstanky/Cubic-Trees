import scala.collection.mutable.Queue

object Root {
	def findNeighbors(adjMat: Array[Array[Int]], node: Int): List[Int] = 
		adjMat(node).zipWithIndex.filter(_._1 == 1).map(_._2).toList

	def toAdjList(adjMat: Array[Array[Int]]): Array[List[Int]] = 
		adjMat.map(x => x.zipWithIndex.filter(_._1 == 1).map(_._2).toList)

	def findRoot(adjMat: Array[Array[Int]], start: Int): Int = {
		val adjList = toAdjList(adjMat)
		def findFurthest(cur: Int, last: List[Int], dist: Int): (Int, List[Int],Int) = {
			val head = try{
				last.head
			} catch {
				case _ => -1
			}
			val neighs = adjList(cur).filter(_!=head)
			if(neighs.isEmpty) (cur,cur :: last,dist)
			else {
				neighs.map(findFurthest(_,cur :: last,dist+1)).sortWith(_._3 > _._3).head
			}
		}
		val a = findFurthest(0,List.empty[Int],0)
		val b = findFurthest(a._1,List.empty[Int],0)
		val diameter = b._3 
		val median = diameter/2
		b._2(median)
	}
	def makeRootedMatrix(adjMat: Array[Array[Int]]): Array[Array[Int]] = {
		var root = findRoot(adjMat,0)
		val out = adjMat.map(_.clone()).clone()
		def addDirections(node: Int, last: Int): Unit = {
			val neighs = findNeighbors(out, node)
			if(last == -1) neighs.map(x => addDirections(x, node))
			else if(neighs.size == 1) out(node)(last) = 0
			else{
				out(node)(last) = 0
				neighs.filterNot(_ == last).map(x => addDirections(x, node))
			}
		}
		addDirections(root,-1)
		out
	}
	
	def rootCanonicalName(adjMat: Array[Array[Int]]): String = {
		val root = findRoot(adjMat,0)
		val rootedMat = makeRootedMatrix(adjMat)

		def assignCanonicalNames(node: Int): String = {
			if(rootedMat(node).filter(_==1).isEmpty) "10"
			else "1 " + findNeighbors(rootedMat,node).map(child => assignCanonicalNames(child)).sortWith(_.length < _.length).mkString(" ") + " 0"
		}
		assignCanonicalNames(root)
	}

	def ahuTreeIsomorphism(t1: Array[Array[Int]], t2: Array[Array[Int]]): Boolean =
		rootCanonicalName(t1) == rootCanonicalName(t2)
}