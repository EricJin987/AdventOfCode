package Day16
import scala.util.parsing.combinator._
object Day16 extends App with JavaTokenParsers{
	def ingredient = (ident<~":")~wholeNumber ^^ {case a~b => (a,b)}
	def compounds = ident~wholeNumber~":"~>repsep(ingredient,",") ^^ 
					{List() ++ _}
	def figure(a: List[(String,String)],b: Map[String,String]) = a.map(i => b(i._1)).equals(a.map(_._2))
	val auntSue = Map("children"->"3","cats"->"7","samoyeds"->"2","pomeranians"->"3",
						"akitas"->"0","vizslas"->"0","goldfish"->"5","trees"->"3","cars"->"2","perfumes"->"1")
	val input = scala.io.Source.fromFile("1.txt").getLines.toList
	val result = input.map(parse(compounds,_).get)
	val realAunt = result.filter(figure(_,auntSue)).last
	val part1 = result.indexOf(realAunt)+1
	val realAunt2 = result.find(_.forall(l => l._1 match {
		case "cats"|"trees" => l._2 > auntSue(l._1)
		case "pomeranians"|"goldfish" => l._2 < auntSue(l._1)
		case _ => l._2 == auntSue(l._1)
	}))
	val part2 = result.indexOf(realAunt2.get)+1
	println(part1)
	println(part2)
}
