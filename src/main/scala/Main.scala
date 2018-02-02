import TreeGenerator._
import IsomorphismTester._

object Main extends App {
    println("Given the two nonisomorphic cubic trees of order 12")
    println(treeOne.deep.mkString("\n"))
    println()
    println(treeTwo.deep.mkString("\n"))
    println()
    println("We can generate the nonisomorphic trees of order 18")
    val newTrees = growTree(treeTwo) ::: growTree(treeOne)
    val trees14 = parseTrees(newTrees)
    val trees16 = parseTrees(trees14.flatMap(growTree(_)))
    val trees18 = parseTrees(trees16.flatMap(growTree(_)))
    for(i <- trees18){
        println(i.deep.mkString("\n"))
        println("The cubic sequencings are as follows:")
        val sequences = generateSequences(i)
        println(sequences)
        println("With the longest sequence being:")
        println(longestItem(sequences))
        println()
    }
}