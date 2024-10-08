import scala.util.Random
import scala.math._
import java.awt.image.BufferedImage
import java.awt.{Color => AwtColor}
import java.io.File
import javax.imageio.ImageIO


// Grid definitions
case class Point(x: Int, y: Int)
case class Color(r: Int, g: Int, b: Int)

// Terrain and other world-related data structures
object TerrainType extends Enumeration {
  type TerrainType = Value
  val OCEAN, LAND = Value
}

object Main extends App {
  // Constants
  val WIDTH = 256
  val HEIGHT = 256
  val MAX_LAND_HEIGHT = 255
  val SEA_LEVEL = 127
  val ROUGHNESS = 0.7

  import TerrainType._

  // Initialize Random with a seed
  val random = new Random(System.currentTimeMillis())

  // Helper Functions
  // Generate a heightmap using the midpoint displacement algorithm
  // Generate a heightmap using the midpoint displacement algorithm
  def midpointDisplacement(size: Int, roughness: Double): Array[Array[Double]] = {
    val terrain = Array.fill(size, size)(0.0)

    def displace(size: Int): Double =
      (Random.nextDouble() - 0.5) * size * roughness

    // Initialize the four corners
    terrain(0)(0) = random.nextDouble() * MAX_LAND_HEIGHT
    terrain(0)(size - 1) = random.nextDouble() * MAX_LAND_HEIGHT
    terrain(size - 1)(0) = random.nextDouble() * MAX_LAND_HEIGHT
    terrain(size - 1)(size - 1) = random.nextDouble() * MAX_LAND_HEIGHT

    // Midpoint displacement recursive function
    def divide(size: Int): Unit = {
      if (size > 1) {
        val half = size / 2

        // Square step
        for (x <- half until WIDTH by size; y <- half until HEIGHT by size) {
          val y_plus_half = (y + half).min(255)
          val x_plus_half = (x + half).min(255)

          val avg = (terrain(x - half)(y - half) +
                     terrain(x - half)(y_plus_half) +
                     terrain(x_plus_half)(y - half) +
                     terrain(x_plus_half)(y_plus_half)) / 4

         terrain(x)(y) = avg + displace(size)
        }

        // Diamond step
        for (x <- 0 until WIDTH by half) {
          for (y <- ((x + half) % size) until HEIGHT by size) {
            val avg = (
              terrain((x - half + WIDTH) % WIDTH)(y) +
                terrain((x + half) % WIDTH)(y) +
                terrain(x)((y + half) % HEIGHT) +
                terrain(x)((y - half + HEIGHT) % HEIGHT)) / 4
            terrain(x)(y) = avg + displace(size)
          }
        }

        // Recursively divide the grid
        divide(size / 2)
      }
    }

    // Start recursion
    divide(size)

    // Normalize heights
    val min = terrain.flatten.min
    val max = terrain.flatten.max
    for (x <- 0 until WIDTH; y <- 0 until HEIGHT) {
      terrain(x)(y) = ((terrain(x)(y) - min) / (max - min)) * MAX_LAND_HEIGHT
    }

    terrain
  }

  def distance(p1: Point, p2: Point): Double =
    sqrt(pow(p1.x - p2.x, 2) + pow(p1.y - p2.y, 2))

  // Generate random terrain height
  def generateTerrain(): Array[Array[Int]] = {
    val terrain = Array.ofDim[Int](WIDTH, HEIGHT)
    for (x <- 0 until WIDTH; y <- 0 until HEIGHT) {
      terrain(x)(y) = Random.nextInt(MAX_LAND_HEIGHT)
    }
    terrain
  }

  // Determine if point is land or ocean
  def determineTerrainType(height: Int): TerrainType =
    if (height > SEA_LEVEL) LAND else OCEAN

  // Color map based on height
  def getColor(height: Double): Color =
    if (height <= SEA_LEVEL) Color(0, 0, 255) // Ocean: Blue
    else if (height <= SEA_LEVEL + 20) Color(0, 255, 0) // Land near sea level: Green
    else if (height <= SEA_LEVEL + 50) Color(139, 69, 19) // Low land
    else Color(255, 255, 255) // High peaks: White

  // Generate the world using midpoint displacement
  def generateWorld(): Array[Array[Color]] = {
    val terrain = midpointDisplacement(WIDTH, ROUGHNESS)
    val world = Array.ofDim[Color](WIDTH, HEIGHT)

    for (x <- 0 until WIDTH; y <- 0 until HEIGHT) {
      val height = terrain(x)(y)
      world(x)(y) = getColor(height)
    }

    world
  }

  // Print world as a grid (simplified visual output)
  def printWorld(world: Array[Array[Color]]): Unit = {
    for (y <- 0 until HEIGHT) {
      for (x <- 0 until WIDTH) {
        world(x)(y) match {
          case Color(0, 0, 255) => print("~") // Ocean
          case Color(0, 255, 0) => print(".") // Low Land
          case Color(139, 69, 19) => print("^") // High Land
          case _ => print(" ")
        }
      }
      println()
    }
  }

  // Save generated world to a PNG file
  def saveWorldAsImage(world: Array[Array[Color]], filename: String): Unit = {
    // Create a BufferedImage
    val image = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB)

    // Set pixels based on the generated world color map
    for (x <- 0 until WIDTH; y <- 0 until HEIGHT) {
      val color = world(x)(y)
      val awtColor = new AwtColor(color.r, color.g, color.b)
      image.setRGB(x, y, awtColor.getRGB)
    }

    // Save image to file
    val outputFile = new File(filename)
    ImageIO.write(image, "png", outputFile)
  }

  val world = generateWorld()
  saveWorldAsImage(world, "world_map.png")
  printWorld(world)
}