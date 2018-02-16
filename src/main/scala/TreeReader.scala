import scala.io.Source

object TreeReader{
    val fileStream = getClass.getResourceAsStream("/unread.txt")
    val lines = Source.fromInputStream(fileStream).getLines    
}