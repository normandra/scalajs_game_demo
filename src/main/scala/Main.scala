import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import scala.util.Random
import scala.scalajs.js
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement


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

class Paddle(var pos: Point, var dims: Point, var direction: Point, val face: Double)
class Ball(var pos: Point, var velocity: Point,var size: Point)

object Main {

  def main(args: Array[String]): Unit = {
    initGame
  }

  def initGame(): Unit = {
    // Create the canvas
    val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    val maxWidth = (0.95 * dom.window.innerWidth).toInt
    val maxHeight = (0.95 * dom.window.innerHeight).toInt

    canvas.width = maxWidth
    canvas.height = maxHeight
    dom.document.body.appendChild(canvas)

    // PONG-----------

    val leftPaddle = new Paddle(Point(maxWidth/20, maxHeight/2), Point(maxWidth/40, maxHeight/5), Point(0, 0), 0.5)
    val rightPaddle = new Paddle(Point(maxWidth - maxWidth/20 - maxWidth/40 , maxHeight/2), Point(maxWidth/40, maxHeight/5), Point(0, 0), -0.5)

    val ball = new Ball(Point(maxWidth/2,maxHeight/2), Point(8 * (Random.nextInt(2) - 0.5), 8 * (Random.nextInt(2) - 0.5)),Point(maxWidth/40,maxWidth/40))

    def doPaddleCollision(paddle: Paddle) = {
      val corner1 = paddle.pos
      val corner2 = paddle.pos + paddle.dims
      if (ball.pos.within(corner2, corner1, extra = Point(5, 5))){
        ball.velocity = ball.velocity.copy(
          x = 2 * paddle.face * math.abs(ball.velocity.x),
          y = ball.velocity.y + paddle.direction.y / 8
        )
      }
    }

    // PONG-----------

    // Handle keyboard controls
    import scala.collection.mutable.HashMap
    val keysDown = HashMap[Int, Boolean]()

    dom.window.addEventListener("keydown", (e: dom.KeyboardEvent) => {
      keysDown += e.keyCode -> true
    }, false)

    dom.window.addEventListener("keyup", (e: dom.KeyboardEvent) => {
      keysDown -= e.keyCode
    }, false)

    // Reset the game
    def reset() = {
      ball.pos = Point(maxWidth/2,maxHeight/2)
      ball.velocity = Point(8 * (Random.nextInt(2) - 0.5), 8 * (Random.nextInt(2) - 0.5))

      leftPaddle.pos = Point(maxWidth/20, maxHeight/2)
      rightPaddle.pos = Point(maxWidth - maxWidth/20 - maxWidth/40 , maxHeight/2)
    }

    // Update
    def update(modifier: Double): Unit = {
      doPaddleCollision(leftPaddle)
      doPaddleCollision(rightPaddle)


      //input and paddle
      if (keysDown.contains(KeyCode.W)) leftPaddle.pos -= Point(0, maxWidth/100)
      if (keysDown.contains(KeyCode.S)) leftPaddle.pos += Point(0, maxWidth/100)

      if (keysDown.contains(KeyCode.Up)) rightPaddle.pos -= Point(0, maxWidth/100)
      if (keysDown.contains(KeyCode.Down)) rightPaddle.pos += Point(0, maxWidth/100)



      //update ball
      ball.pos += ball.velocity

      if (ball.pos.y <= 0) ball.velocity = ball.velocity.copy(y = math.abs(ball.velocity.y))
      if (ball.pos.y >= maxHeight) ball.velocity = ball.velocity.copy(y = -math.abs(ball.velocity.y))

      if (ball.pos.x <= 0 || ball.pos.x >= maxWidth) reset()

    }

    // Draw
    def render(x: Int, y: Int): Unit = {
      //
      ctx.fillStyle = Color.Black
      ctx.fillRect(0, 0, x, y)

      ctx.fillStyle = Color.White
      ctx.fillRect(leftPaddle.pos.x, leftPaddle.pos.y
        , leftPaddle.dims.x, leftPaddle.dims.y)

      ctx.fillRect(rightPaddle.pos.x, rightPaddle.pos.y
        ,rightPaddle.dims.x, rightPaddle.dims.y)

      ctx.fillRect(ball.pos.x,ball.pos.y
        ,ball.size.x,ball.size.y)

    }

    var prev = js.Date.now()

    // The main game loop
    val gameLoop = () => {
      val now = js.Date.now()
      val delta = now - prev

      update(delta / 1000)
      render(maxWidth,maxHeight)

      prev = now
    }
    reset()

    dom.window.setInterval(gameLoop, 1)
  }
}
