import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.RenderingHints
import java.awt.image.Raster
import java.awt.Rectangle

// Based on the article (but not source) from 
// http://www.drdobbs.com/184404848

// Logically a few steps:
// Create an index out of all the photos you want to consider; this index
// consists of a mapping from image file to average color in the image

// For the source image, examine rectangular regions of the image.  Replace
// these regions with the picture calculated in the index with the most similar
// average color.
//
// Output the photomosaic to a file


// TODO: take into account the repetition of the images; set a limit on number of
// times an image can appear in the final product
// TODO: integrate with iPhoto
// TODO: integrate with flickr and/or Picasa
// TODO: take arguments for the size, format of the output image
// 
// TODO: progress reporting
// TODO: infinite photomosaic with zoom

package net.developmentality.photo {


// type PhotoMetadata = Tuple3[File,Color,BufferedImage]

// class PhotoMetadata(originalFile:File, imageColor:Color, shrunkenSwatch:BufferedImage) {}

// class PhotoIndex(Map[File, PhotoMetadata]) {
//   def toCsv():String
// }



/**
 * The photo index is a plain text, CSV file consisting of 
 * the original photo location, the average color of the image, and the path
 * to a shrunk down version of the image.  
 * 
 * In the index creation process, the images are
*/
object PhotoIndexer {
  
  type PhotoMetadata = Tuple3[File,Color,BufferedImage]
  type PhotoIndex = Map[File, PhotoMetadata]
  
  
  def loadIndex(indexLoc:File):PhotoIndex = {
    null
  }
  

  /**
  * Creates an 
  */
  def createIndex(images:Seq[File], outputLoc:File, 
    thumbnailOutput:File,
    width:Int, height:Int):PhotoIndex = {

      
      // Create the BufferedImage, calculate average color
      images.map(x => 
        {
          val buffImg:BufferedImage = ImageIO.read(x)
          val avgColor:Color = PhotoMosaic.calculateColor(buffImg)
          val shrunken:BufferedImage = shrinkSwatch(buffImg, width, height)
        
        }
      )
      null
  }
  
  def createRow(file:File):PhotoMetadata = {
    null
  }
  
  
  def shrinkImage(file:File): Unit = {
    println("Shrinking image " + file)
    
    val buffImage = ImageIO.read(file)
    val shrunken = shrinkSwatch(buffImage, 20, 20)
    
    // Chop off the extension and add a small suffix
    val dotIndex = file.getPath().lastIndexOf(".")
        
    ImageIO.write(shrunken, "png", new File(file.getPath().slice(0,dotIndex) + "_small.png") )
  }
  
  /** Shrinks an image to given dimensions.  Aspect ratio is not preserved.
   @see {http://helpdesk.objects.com.au/java/how-do-i-scale-a-bufferedimage}
  */
  def shrinkSwatch(original:BufferedImage, width:Int, height:Int) = {
    val scaledImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val graphics2D = scaledImage.createGraphics()
    
    graphics2D.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
      RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      
    graphics2D.drawImage(original, 0, 0, width, height, null)
    graphics2D.dispose()
    scaledImage
  }
  
  object PhotoIndex {
    

  }
  
}




object PhotoMosaic {
  
  implicit def color2RichColor(x: Color) = new RichColor(x)
  
  val sampleSize = 40
    
    
  // TODO: Add command line arguments
  // -i, in, index = index file
  // -t, target = target image file (must be a format that ImageIO can read)
  // -pix [picture1 .. pictureN]
  def main(args: Array[String]) {
    val files = args.map(new File(_))
    
    val indexLoc = files.findIndexOf(_.getName().equals("index.txt"))
    
    val index:Seq[Tuple2[File,Color]] =
      // Index already exists
      if (indexLoc >= 0) {
        loadIndex(args(indexLoc))
      }
      else {
        createIndex(files)
      }
    
    val target = files(0)
    val mosaic:BufferedImage = photoMosaicize(target, index)
    // ImageIO.write(mosaic,"png",new File("testmosaic.png"))
    
    null
  }
  
  // TODO: use a class to represent the index
  def photoMosaicize(targetFile:File, index:Seq[Tuple2[File,Color]]): BufferedImage = {
    
    val buffImage = ImageIO.read(targetFile)
    val patchSampleSize = 30
    val patchWidth = patchSampleSize
    val patchHeight = patchSampleSize
        
    val targetSquareSize = 20
    
        
    val numHorizontalPatches = buffImage.getWidth() / patchWidth
    val numVerticalPatches = buffImage.getHeight() / patchHeight
    
    val mosaic = new BufferedImage(targetSquareSize * numHorizontalPatches, targetSquareSize * numVerticalPatches, BufferedImage.TYPE_INT_RGB)
    val graphics2D = mosaic.createGraphics()

    val imageMap = createSmallImageMap(index)

    var counter = 1
    val numSubImages = numHorizontalPatches * numVerticalPatches
    
    // for each patch in the image
    for (i <- 0 until numHorizontalPatches) {
      for (j <- 0 until numVerticalPatches) {
        val x = i * patchWidth
        val y = j * patchHeight
        val subImage = buffImage.getData(new Rectangle(x,y,patchWidth,patchHeight))
        val avgImageColor = calculateColorFromRaster(subImage)
        
        // This way merely draws the block of average color
        // graphics2D.setColor(avgImageColor)
        // graphics2D.fillRect(2*x,2*y,20,20)
        

        var x2 = i * targetSquareSize
        var y2 = j * targetSquareSize
        val nearest = getNearestColorImage(avgImageColor, imageMap)
        
        graphics2D.drawImage(nearest, x2, y2, targetSquareSize, targetSquareSize, null)

        val percent = 100.0 * counter / numSubImages
        // TODO: for GUI version, use a display bar
        if (counter % 100 == 0) {
          println(percent + " completed (" + counter + " of" + numSubImages + ")")
        }
        counter+=1
        

      }
    }
    mosaic
  }
  
  
  def createSmallImageMap(colorMap:Seq[Tuple2[File,Color]]): Map[BufferedImage,Color] = {
    var map:Map[BufferedImage,Color] = Map()
    for (x <- colorMap) {
      val theFile = x._1
      val theColor = x._2
      val filePath = theFile.getPath().replace(".JPG","_small.png")
      println("Adding " + filePath + " with color " + theColor + " to map.")
      map += (ImageIO.read(new File(filePath)) -> theColor)
      null
    }
    map
  }
  
  // Sort the Colors 
  
  // Calculates the BufferedImage with the single best 
  def getNearestColorImage(color: Color, colorMap:Map[BufferedImage,Color]): BufferedImage = {
    // Find image with closest average color
    val elements:List[(BufferedImage,Color)] = colorMap.elements.toList
    val closestElem = elements.reduceLeft(
      (e1,e2) => (if (color.distance(e1._2) < color.distance(e2._2)) e1 else e2)
    )
    closestElem._1
  }
  
  /**
  * Calculates the 
  */
  def getNearestColorImages(targetColor: Color, colorMap:Map[BufferedImage, Color]): Seq[BufferedImage] = {
    val keys:List[BufferedImage] = colorMap.keys.toList
    keys.sort((c1,c2) =>
      colorMap(c1).distance(targetColor) < colorMap(c2).distance(targetColor)
    )
    keys
  }
 
  
  // Given a sequence of colors, sort them in order of proximity to the target
  // color
  def sortColors(colors:List[Color], targetColor:Color): List[Color] = {
    colors.sort( (c1,c2) =>
      c1.distance(targetColor) < c2.distance(targetColor)
    )
  }
  
  // Creates a solid swatch of color matching average of 
  def createSwatch(color:Color): BufferedImage = {
    val width, height = 10
    val img = new BufferedImage(width,height,BufferedImage.TYPE_INT_ARGB)
    val graphics = img.createGraphics()
    
    graphics.setColor(color)
    graphics.fillRect(0,0,width,height)
    img
  }

  // Given the path to an index file, reads in all the text and extracts the
  // saved data
  def loadIndex(indexLoc:String): Seq[Tuple2[File, Color]] = {
    // An iterator of lines in the file; note that they have newline control characters
    val lines = scala.io.Source.fromFile(indexLoc).getLines
    
    // Each line already consists of a file, color 
    lines.map(extractFileAndColor).toList
    
  }
  
  // Lines are csv: file, red, green, blue
  def extractFileAndColor(indexString:String) : Tuple2[File,Color] = {
    // remove newline, 
    val trimmed = indexString.trim()
    val values = trimmed.split(",")
    val fileLoc = values(0).toString()
    val (red, green, blue) = (values(1).toInt, values(2).toInt, values(3).toInt)
    (new File(fileLoc), new Color(red, green,blue))
  }
  
  def createIndex(files: Seq[File]): Seq[Tuple2[File, Color]] = {
    // Calculate the average color of each image
    val indices = files.map( 
      x=> (x, calculateColor( ImageIO.read(x) ) ) 
    )
    indices
  }
  
  // A raster is an abstraction of a piece of an image and the underlying
  // pixel data.
  // For instance, we can get a raster than is of the upper left twenty
  // pixel square of an image
  def calculateColorFromRaster(raster:Raster): Color = {
    var redSum = 0
    var greenSum = 0
    var blueSum = 0
  
    val minX = raster.getMinX()
    val minY = raster.getMinY()
  
    val height = raster.getHeight()
    val width = raster.getWidth()
    val numPixels = height * width
  
    val numChannels = raster.getNumBands() 
  
    val pixelBuffer = new Array[Int](width*height*numChannels)
    val pixels = raster.getPixels(minX,minY,width,height,pixelBuffer)
  
    // pixelBuffer now filled with r1,g1,b1,r2,g2,b2,...
    // If there's an alpha channel, it will be r1,g1,b1,a1,r2,... but we skip the alpha
    for (i <- 0 until numPixels) {
      val redOffset = numChannels * i
      val red = pixels(redOffset)
      val green = pixels(redOffset+1)
      val blue = pixels(redOffset+2)
    
      redSum+=red
      greenSum+=green
      blueSum+=blue
    }
    new Color(redSum / numPixels, greenSum / numPixels, blueSum / numPixels)
  }
  
  // def calculateColorFromRaster(raster: Raster): Color = {
  //     val minX = raster.getMinX()
  //     val minY = raster.getMinY()
  //   
  //     val height = raster.getHeight()
  //     val width = raster.getWidth()
  //     val numPixels = height * width
  //   
  //     val numChannels = raster.getNumBands() 
  //   
  //     val pixelBuffer = new Array[Int](width*height*numChannels)
  //     val pixels = raster.getPixels(minX,minY,width,height,pixelBuffer)
  //     
  //     
  //   }

  def calculateColor(f:BufferedImage): Color = {
    calculateColorFromRaster(f.getRaster())
  }
    
}


/**
* Enhances Color objects with the ability to determine a distance measure
* from another color.  
*/
class RichColor(original:Color) {
  implicit def color2RichColor(original: Color) = new RichColor(original)
  
  
  // Avoid taking a square root unnecessarily
  def euclideanDistanceSquared(other:Color):Double = {
    val (r1,g1,b1) = unpack()
    val (r2,g2,b2) = other.unpack()
    (r1-r2) * (r1-r2) + (g1-g2) * (g1-g2) + (b1-b2) * (b1-b2)
  }
  
  def euclideanDistance(other:Color):Double = {
    val squared = euclideanDistanceSquared(other)
    Math.sqrt(squared)
  }
 
  /** Currently define the distance function as the squared euclidean distance, but this can be changed*/
  val distance : (Color) => Double = euclideanDistanceSquared
 
  /** @return a Tuple3 where order is red, green, blue and numbers are in range 0..255 */
  def unpack(): Tuple3[Int,Int,Int] = {
    (original.getRed(), original.getGreen(), original.getBlue())
  }
}


}