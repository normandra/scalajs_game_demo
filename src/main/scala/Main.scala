import java.util.Dictionary

import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode

import scala.util.Random
import scala.scalajs.js
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object State  {
  val no_spoon = 0
  val yes_spoon = 1
  val no = 2
  val yes = 3
  val all = Seq(
    yes,
    yes_spoon,
    no,
    no_spoon
  )
}

object Color {
  def rgb(r: Int, g: Int, b: Int) = s"rgb($r, $g, $b)"

  val White = rgb(255, 255, 255)
  val Red = rgb(255, 0, 0)
  val Green = rgb(0, 255, 0)
  val Blue = rgb(0, 0, 255)
  val Cyan = rgb(0, 255, 255)
  val Magenta = rgb(255, 0, 255)
  val Yellow = rgb(255, 255, 0)
  val Black = rgb(0, 0, 0)
  val all = Seq(
    White,
    Red,
    Green,
    Blue,
    Cyan,
    Magenta,
    Yellow,
    Black
  )
}

class SpatialHashManager(width: Int, height: Int, cellSize: Int){
  val Cols= width / cellSize;
  val Rows= height / cellSize;
  val Buckets = new mutable.HashMap[Int,ListBuffer[Ball]]()

  var iii = 0
  for (iii <- 0 to Cols * Rows)
  {
    Buckets += (iii -> ListBuffer())
  }

  val SceneWidth = width;
  val SceneHeight = height;
  val CellSize = cellSize;
  val cellWidth = SceneWidth / CellSize

  def ClearBuckets(): Unit ={

    Buckets.clear()
    var i = 0
    for (i <- 0 to Cols * Rows)
    {
      Buckets += (i -> ListBuffer())
    }
  }

  def RegisterObject(ball: Ball): Unit ={
    val cellIds = GetIdForObj(ball)
//    cellIds.foreach(id => Console.println(id))
//    Console.println(cellIds.toString())
    cellIds.foreach(id => {
//      Console.println(Buckets.size)
        Buckets(id) += ball
    })
  }

  def GetIdForObj(ball: Ball): ListBuffer[Int] ={
    val bucket = ListBuffer[Int]()

    val minPoint = new Point(ball.pos.x - ball.size,ball.pos.y - ball.size)
    val maxPoint = new Point(ball.pos.x + ball.size,ball.pos.y + ball.size)


    AddBucket(minPoint, cellWidth, bucket)
    //TopRight
    AddBucket(new Point(maxPoint.x, minPoint.y), cellWidth, bucket)
    //BottomRight
    AddBucket(maxPoint, cellWidth, bucket)
    //BottomLeft
    AddBucket(new Point(minPoint.x, maxPoint.y), cellWidth, bucket)

    return bucket
  }

  def AddBucket(point: Point,float: Float,list: ListBuffer[Int]): Unit ={
    val cellPosition = Math.floor(point.x / CellSize) + Math.floor(point.y / CellSize) * float
    if(cellPosition > Buckets.size || cellPosition < 0){
      Console.println(Buckets.size + " Buckets element")
      Console.println("cell pos " + cellPosition + ", x:" + point.x + " y:" + point.y + " cellsize:" + cellSize + " float:" + float + " screenwidth:" + width + " screenheight:" + height)
    }
    if (!list.contains(cellPosition)){
      list += cellPosition.toInt
    }
  }

  def GetNearby(ball: Ball): ListBuffer[Ball] = {

//    if(Buckets.isEmpty){
//      Console.println("empty bucket")
//    }else{
//      Console.println(Buckets.size)
//    }

    val items = ListBuffer[Ball]()
    val bucketIds = GetIdForObj(ball)
//    println(bucketIds.toString())
    bucketIds.foreach(id => {
      if(Buckets(id).length > 1)
//      println("Buckets id " + Buckets(id).toString())
//      Console.println("called")
//      if (id > 0 && id < Buckets.size)
        Buckets(id).foreach(element => items += element)
    })

    items
  }

  def CheckCollision(ball: Ball,list: ListBuffer[Ball]): Unit ={


    for (a <- list){

      if (a != ball){

        val distance = Math.sqrt(Math.pow(ball.pos.x - a.pos.x, 2) + Math.pow(ball.pos.y - a.pos.y, 2))

        if(distance < ball.size ){
//          Console.println("collision triggered")
          // collision happens
          if(ball.cooldown <= 0){
            if(a.state == State.yes_spoon && ball.state == State.no_spoon){
              a.state = State.yes
              ball.state = State.no
              ball.cooldown = 1000
            }else if (a.state == State.yes_spoon && ball.state == State.no){
              a.state = State.yes
              ball.state = State.yes_spoon
              ball.cooldown = 1000
            }else if (a.state == State.yes && ball.state == State.no_spoon){
              a.state = State.no_spoon
              ball.state = State.no
              ball.cooldown = 1000
            }
          }
        }
      }
    }
  }

}

case class Point(x: Double, y: Double){
  def +(other: Point) = Point(x + other.x, y + other.y)
  def -(other: Point) = Point(x - other.x, y - other.y)
  def %(other: Point) = Point(x % other.x, y % other.y)
  def <(other: Point) = x < other.x && y < other.y
  def >(other: Point) = x > other.x && y > other.y
  def /(value: Double) = Point(x / value, y / value)
  def *(value: Double) = Point(x * value, y * value)
  def *(other: Point) = x * other.x + y * other.y
  def length = Math.sqrt(lengthSquared)
  def lengthSquared = x * x + y * y
  def within(a: Point, b: Point, extra: Point = Point(0, 0)) = {
    import math.{min, max}
    x >= min(a.x, b.x) - extra.x &&
      x < max(a.x, b.x) + extra.y &&
      y >= min(a.y, b.y) - extra.x &&
      y < max(a.y, b.y) + extra.y
  }
  def rotate(theta: Double) = {
    val (cos, sin) = (Math.cos(theta), math.sin(theta))
    Point(cos * x - sin * y, sin * x + cos * y)
  }
}

class Ball(var pos: Point, var velocity: Point,var size: Int,var state:Int,var cooldown: Double)

object Main {

  def main(args: Array[String]): Unit = {
    initGame
  }

  def initGame(): Unit = {

    //init vars

    // Create the canvas
    val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    val maxWidth = (0.95 * dom.window.innerWidth).toInt
    val maxHeight = (0.95 * dom.window.innerHeight).toInt

    val ball_size = 20

    canvas.width = maxWidth
    canvas.height = maxHeight
    dom.document.body.appendChild(canvas)

    val num_balls = 2000
    //    val balls_array = Array.fill[Ball](num_balls)(new Ball(Point(maxWidth/2,maxHeight/2), Point(8 * (Random.nextInt(4) - 0.5), 8 * (Random.nextInt(4) - 0.5)),30,Random.nextInt(4)))
    var balls_array = ListBuffer[Ball]()

    for (_ <- 1 to num_balls) {
      balls_array += new Ball(Point(Random.nextInt(maxWidth - ball_size * 8) + ball_size * 4
        ,Random.nextInt(maxHeight - ball_size * 8) + ball_size * 4), Point(8 * (Random.nextInt(4) - 0.5) / 10, 8 * (Random.nextInt(4) - 0.5) / 10),ball_size,Random.nextInt(2) ,0)
    }


    val spatialHashManager = new SpatialHashManager(maxWidth,maxHeight,50)

    // Reset the game
    def reset() = {

    }

    // Update
    def update(modifier: Double): Unit = {
      Console.println(modifier)

      //clear shm
      spatialHashManager.ClearBuckets()

      //update ball
      for (ball <- balls_array) {
        update_ball(ball, modifier)
//        ball.state = Random.nextInt(4)
        spatialHashManager.RegisterObject(ball)
      }

      for (ball <- balls_array) {
//        Console.println("get nearby " +  spatialHashManager.GetNearby(ball).toString())
//        spatialHashManager.CheckCollision(ball,balls_array)
        spatialHashManager.CheckCollision(ball,spatialHashManager.GetNearby(ball))
      }


    }

    def update_ball(ball: Ball, delta: Double): Unit = {
      ball.pos += ball.velocity
      if (ball.cooldown > 0 ) ball.cooldown -= delta

      if (ball.pos.y <= ball.size * 2) ball.velocity = ball.velocity.copy(y = math.abs(ball.velocity.y))
      if (ball.pos.y >= maxHeight - ball.size * 2) ball.velocity = ball.velocity.copy(y = -math.abs(ball.velocity.y))

      if (ball.pos.x <= ball.size * 2) ball.velocity = ball.velocity.copy(x = math.abs(ball.velocity.x))
      if (ball.pos.x >= maxWidth - ball.size * 2) ball.velocity = ball.velocity.copy(x = -math.abs(ball.velocity.x))
    }

    // Draw
    def render(x: Int, y: Int): Unit = {
      //
      ctx.fillStyle = Color.Black
      ctx.fillRect(0, 0, x, y)

      for (ball <- balls_array) draw_ball(ball)

      //spatial hash map draw
      val cell = maxWidth / spatialHashManager.CellSize
      val hori = maxHeight / spatialHashManager.CellSize

//      ctx.strokeStyle = Color.Yellow
//      ctx.lineWidth = 10
//
//      var n = 0
//      for (n <- 0 to cell by spatialHashManager.CellSize){
//        ctx.beginPath();
//        ctx.moveTo(n,0)
//        ctx.lineTo(n,maxHeight)
//        ctx.stroke()
//      }
//
//      for (n <- 0 to hori by spatialHashManager.CellSize){
//        ctx.beginPath();
//        ctx.moveTo(0,n)
//        ctx.lineTo(maxWidth,n)
//        ctx.stroke()
//      }

    }

    def draw_ball(ball: Ball): Unit = {


      ball.state match {
        case 0 =>
          ctx.fillStyle = Color.White
          ctx.strokeStyle = Color.White

        case 1 =>
          ctx.fillStyle = Color.Red
          ctx.strokeStyle = Color.Red

        case 2 =>
          ctx.fillStyle = Color.Blue
          ctx.strokeStyle = Color.Blue

        case 3 =>
          ctx.fillStyle = Color.Green
          ctx.strokeStyle = Color.Green

      }

      ctx.beginPath()
      ctx.arc(ball.pos.x,ball.pos.y,ball.size,0,2*Math.PI)
      ctx.stroke()
      ctx.closePath()
      ctx.fill()
    }

    var prev = js.Date.now()

    // The main game loop
    val gameLoop = () => {
      val now = js.Date.now()
      val delta = now - prev

      update(delta)
      render(maxWidth,maxHeight)

      prev = now
    }
    reset()

    dom.window.setInterval(gameLoop, 10)
  }
}
