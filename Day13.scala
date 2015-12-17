package Day13
import scala.io.Source
import scala.util.matching
object Day13 extends App{
	val gain = """(\w+).+gain (\d+).+to (\w+).""".r
	val lose = """(\w+).+lose (\d+).+to (\w+).""".r
	def getCircles(arr: List[String]):List[List[String]]={
		if(arr.size==1) List.fill(1)(arr)
		else arr.tail.permutations.map(arr.head::_).toList
	}
	def getGuests(m: Map[(String,String),Int]):List[String] = m.keys.map(_._1).toList.distinct
	def seat(arr:List[List[String]],m: Map[(String,String),Int]):Int = arr.map(l => l.sliding(2).toList.
			foldLeft(m(l.head,l.last)+m(l.last,l.head))((b,a) => b+m(a(0),a(1))+m(a(1),a(0)))).max
	val happiness = Source.fromFile("1.txt").getLines().map(line => line match{
				case gain(a,units,b) => (a,b)->units.toInt
				case lose(a,units,b) => (a,b)->units.toInt.unary_-.toInt}).toMap

	//part1
	println(seat(getCircles(guests),happiness))
	val guests = getGuests(happiness)
  val happiness2 = happiness++(guests.map((_,"me")->0))++(guests.map(("me",_)->0))
	//part2
	println(seat(getCircles(getGuests(happiness2)),happiness2))
}
