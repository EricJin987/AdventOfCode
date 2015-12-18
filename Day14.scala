package Day14
import scala.io.Source
object Day14 extends App{
	def compete(l: List[(Int,Int,Int)], deadline: Int):List[Int]= {
		def dis(seq : (Int,Int,Int)):Int ={
			val a = seq._1*seq._2*(deadline/(seq._2+seq._3))
			val b = deadline%(seq._2+seq._3)
			a + (if (b > seq._2) seq._1*seq._2 else seq._1*b)
		}
		l.map(dis)
	}
	val info = for {line <- Source.fromFile("1.txt").getLines().toList
					s = line.split(" ")
					reindeer = (s(3).toInt,s(6).toInt,s(13).toInt)} yield reindeer
					
	def compete2(list: List[(Int,Int,Int)],deadline: Int):List[List[(Int,Int,Int)]] = {
		for {i <- (1 to deadline).toList
			dis = compete(list,i)
			top = list.zip(dis).filter(_._2 == dis.max).map(_._1)
		} yield top
	}
	//part1
	println(compete(info,2503).max)
	//part2
	println(compete2(info,2503).flatten.groupBy(identity).values.map(_.size).max)
}
