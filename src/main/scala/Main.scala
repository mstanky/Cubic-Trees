import TreeGenerator._
import IsomorphismTester._
import DatWriter._
import TreeReader._
import java.io._

object Main extends App {
    val newTrees = growTree(treeTwo) ::: growTree(treeOne)
    val trees14 = parseTrees(newTrees)
    val trees16 = parseTrees(trees14.flatMap(growTree(_)))
    val trees18 = parseTrees(trees16.flatMap(growTree(_)))
    val trees20 = parseTrees(trees18.flatMap(growTree(_)))
    println(trees20.head.size)
    val trees22 = parseTrees(trees20.flatMap(growTree(_)))
    val trees24 = parseTrees(trees22.flatMap(growTree(_)))
    val trees26 = parseTrees(trees24.flatMap(growTree(_)))

    val trees14List = trees14.map(toAdjList(_))
    var title = "outputs/14nodesNum"
  	var num = 1
	  for(tree <- trees14List){
		    val pw = new PrintWriter(new File(title + num.toString + ".dat"))
		    pw.write(writeOut(tree, bipartition(tree)))
		    pw.close
		    num += 1
	  }
	
  	title = "outputs/20nodesNum"
    num = 1
    for(tree <- trees20.map(toAdjList(_))){
		    val pw = new PrintWriter(new File(title + num.toString + ".dat"))
		    pw.write(writeOut(tree,bipartition(tree)))
		    pw.close
		    num += 1
	  }
	  title = "outputs/22nodesNum"
	  num = 1
	  for(tree <- trees22.map(toAdjList(_))){
		    val pw = new PrintWriter(new File(title + num.toString + ".dat"))
		    pw.write(writeOut(tree,bipartition(tree)))
		    pw.close
		    num += 1
	  }
	  title = "outputs/24nodesNum"
	  num = 1
	  for(tree <- trees24.map(toAdjList(_))){
		    val pw = new PrintWriter(new File(title + num.toString + ".dat"))
		    pw.write(writeOut(tree,bipartition(tree)))
		    pw.close
		    num += 1
	  }
	  title = "outputs/26nodesNum"
	  num = 1
	  for(tree <- trees24.map(toAdjList(_))){
		    val pw = new PrintWriter(new File(title + num.toString + ".dat"))
		    pw.write(writeOut(tree,bipartition(tree)))
		    pw.close
		    num += 1
	  }
}