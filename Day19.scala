package Day19
import scala.util.parsing.combinator._
object Day19 extends App with JavaTokenParsers{
	val machineParser = ident~"=>"~ident ^^ {case a~"=>"~b => (a,b)}
	val input = scala.io.Source.fromFile("1.txt").getLines.toList
	val (expression,m) = input.splitAt(input.size-2)
	val machine = expression.map(parse(machineParser,_).get)
	val molecule = m.last.trim
	val part1 = ( for {
					(a,b) <- machine
					replacements = molecule.sliding(a.size).zipWithIndex.collect{
						case (char,x) if (char.equals(a))=> molecule.take(x) + b + molecule.drop(x+char.size)
					 }
	} yield replacements).flatten.distinct.size
	println(part1)
	//Part2,still from reddit
	def step(s: String):String = (for {
		(a,b) <- machine
		replacements = s.sliding(b.size).zipWithIndex.collect{
			case (char,x) if (char.equals(b))=> s.take(x) + a + s.drop(x+char.size)
			}
	} yield replacements).flatten.sortBy(_.length).head
	lazy val stream:Stream[String] = Stream.cons(molecule,stream.map(step))
	val part2 = stream.takeWhile(s => !(s.contains("e"))).size
	println(part2)
}
