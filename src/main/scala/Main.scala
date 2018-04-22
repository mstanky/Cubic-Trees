import TwelveCaterpillar._

object Main extends App {
	for(diam <- diams){
		println(diam.deep.mkString("\n"))
		println()
	}
	println(diams.size)
}