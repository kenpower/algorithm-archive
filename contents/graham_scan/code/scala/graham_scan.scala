object GiftWrap {

  case class Point(x: Int, y: Int)

  def grahamScan(gift: List[Point]): List[Point] = {

    def isClockwise(p1: Point, p2: Point, p3: Point): Boolean =
      (p3.y - p1.y) * (p2.x - p1.x) >= (p2.y - p1.y) * (p3.x - p1.x)

    def polarAngle(ref: Point)(point: Point): Double =
        Math.atan2(point.y-ref.y, point.x-ref.x)

    def bottomMost(points: List[Point]): Point = 
      points.reduce((a, b) => if (a.y < b.y) a else b) 

    val start = bottomMost(gift)

    var (hull, sorted) = gift
                            .distinct
                            .filter(_ != start)
                            .sortWith(polarAngle(start)(_) > polarAngle(start)(_))
                            .splitAt(2)

    hull= hull.reverse :+ start

    // can we use sorted.reduce here?
    sorted.foreach( p => {
        while (isClockwise(hull(1), hull(0), p)){
            println("" + p +"---" + hull)
            hull = hull.tail
        }
        hull = p::hull
    })

    hull
    
  }


  def main(args: Array[String]): Unit = {

    val testGift = List(
      (-5, 2),
      (-13, 100),
      (5, 7),
      (-6, -12),
      (-14, -14),
      (-9, 9),
      (-1, -1),
      (100, 100),
      (-10, 11),
      (-13, 100),
      (-6, 15),
      (-6, -8),
      (15, -9),
      (0,0),
      (7, -7),
      (-2, -9),
      (100, -100),
      (6, -5),
      (0, 14),
      (-100,100),
      (-100,-100),
      (2, 8)
    ).map({ case (x, y) => Point(x, y) })

    val hull = grahamScan(testGift)

    println("The points in the wrapping are:")
    hull.foreach(println)

  }
}