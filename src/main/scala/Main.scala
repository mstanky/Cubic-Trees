import TreeGenerator._
import IsomorphismTester._
import DatWriter._
import TreeReader._
import java.io._

object Main extends App {
    val newTrees = growTree(treeTwo) ::: growTree(treeOne)
	println(newTrees.head.size)
    val trees14 = parseTrees(newTrees)
    val trees16 = parseTrees(trees14.flatMap(growTree(_)))
    val trees18 = parseTrees(trees16.flatMap(growTree(_)))

    val trees14List = trees14.map(toAdjList(_))
    var title = "outputs/12nodesNum"
	var num = 1
	for(tree <- trees14List){
		val pw = new PrintWriter(new File(title + num.toString + ".dat"))
		pw.write(writeOut(tree, bipartition(tree)))
		pw.close
		num += 1
	}
}