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


case class Metadata(originalImage:File, avgColor:Color, thumbnail:BufferedImage){}



/**
 * The photo index is a plain text, CSV file consisting of 
 * the original photo location, the average color of the image, and the path
 * to a shrunk down version of the image.  
 * 
 * In the index creation process, the images are
*/
object PhotoIndexer {
  // Careful with pipes - you need to escape the pipe, since it has meaning in regexp
  val FIELD_DELIMITER = "\\|";
  val FILE_NAME_INDEX = 0
  val AVG_COLOR_INDEX = 1
  val SHRUNKEN_IMAGE_INDEX = 2

  type PhotoIndex = Map[File, Metadata]
  
  // Given a file with entries for file name, the int representation of the average
  // color, and the file path for the thumbnail, reconstitutes an index
  def loadIndex(indexLoc:File):PhotoIndex = {
    // An iterator of lines in the file; note that they have newline control characters
    val lines = scala.io.Source.fromFile(indexLoc).getLines
    
    // Each line already consists of a file, color, and thumbnail
    val metadata:Seq[Metadata] = lines.map(parseLine).toList
    // Strip out the originalImage field out of each metadata entry
    val extractedFiles = metadata.map(_.originalImage)
    extractedFiles.zip(metadata).toMap
  }
  
  def parseLine(indexRow:String):Metadata = {
    Console.println("Row: " + indexRow)
    
    val entries = indexRow.split(FIELD_DELIMITER)
    
    Console.println("Entries: " + entries.mkString(","))
    
    val theFile:File = new File(entries(FILE_NAME_INDEX))
    val color:Color = Color.decode(entries(AVG_COLOR_INDEX))
    val shrunkenImageFile:File = new File(entries(SHRUNKEN_IMAGE_INDEX))
    val shrunkenImage:BufferedImage = ImageIO.read(shrunkenImageFile)
    Metadata(theFile, color, shrunkenImage)
  }
  

  /**
  * For each file, calculates the average color and also creates a small thumbnail
  * of each image.  Each of the files should be an image file in the format that
  * ImageIO knows how to read (e.g. png, jpg)
  */
  def createIndex(images:Seq[File], 
    thumbnailWidth:Int, thumbnailHeight:Int):PhotoIndex = {
      val numEntries = images.size
      val metadata = images.zipWithIndex.map(tuple => 
        {
          val file = tuple._1
          val index = tuple._2 
          val buffImg:BufferedImage = ImageIO.read(file)
          val avgColor:Color = PhotoMosaic.calculateColor(buffImg)
          val shrunken:BufferedImage = shrinkSwatch(buffImg, thumbnailWidth, thumbnailHeight)
          
          if ((index + 1) % 1 == 0) {
            val percent = (100.0 * (index + 1))/numEntries
            Console.println(percent + "% done; " + (numEntries - (index + 1)) + " entries remaining")
          }
          
          Metadata(file, avgColor, shrunken)
        }
      )
      images.zip(metadata).toMap:PhotoIndex
  }
  
  def saveIndex(index:PhotoIndex, outputFile:File, thumbnailDir:File):Unit = {
    
    if (!thumbnailDir.exists()) {
      thumbnailDir.mkdirs()
    }
    else if (thumbnailDir.isFile()) {
      throw new IllegalArgumentException("Thumbnail directory " + thumbnailDir + " is a file, not a folder")
    }
    
    
    // Strip out the directory name and then the trailing file extension, tacking on
    // a _thumbnail.png suffix
    val fileNames = index.keys.map(_.getName())
    val thumbnailFiles = fileNames.map(imgFile => 
      new File(thumbnailDir, imgFile.substring(0, imgFile.lastIndexOf(".")) + "_thumbnail.png")
    )
    
    val fileWriter = new java.io.FileWriter(outputFile)
    
    index.values.zip(thumbnailFiles).foreach( tuple => {
      val metadata:Metadata = tuple._1
      val thumbnailFile = tuple._2
      val thumbnailFilepath = thumbnailFile.getPath()
      
      ImageIO.write(metadata.thumbnail, "png", thumbnailFile)
      
      val filePath = metadata.originalImage.getPath()
      val colorStr = String.valueOf(metadata.avgColor.getRGB)
      val entries = List(filePath, colorStr, thumbnailFilepath)
      fileWriter.write(entries.mkString(FIELD_DELIMITER))
      fileWriter.write("\n")
      }
    )
    fileWriter.close
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
      
}




object PhotoMosaic {
  
  implicit def color2RichColor(x: Color) = new RichColor(x)
  
  val sampleSize = 40
  
  val THUMBNAIL_WIDTH = 80
  val THUMBNAIL_HEIGHT = 60
    
    
  // TODO: Add command line arguments
  // -i, in, index = index file
  // -t, target = target image file (must be a format that ImageIO can read)
  // -pix [picture1 .. pictureN]
  def main(args: Array[String]):Unit = {
    if (args.length == 0) {
      Console.println("Usage: ")
      Console.println("PhotoMosaic file1 ... fileN")
      System.exit(1)
    }
    val files = args.map(new File(_))
    
    val indexLoc = files.findIndexOf(_.getName().equals("index.txt"))
    
    val index:PhotoIndexer.PhotoIndex =
      // Index already exists
      if (indexLoc >= 0) {
        PhotoIndexer.loadIndex(new File(args(indexLoc)))
      }
      else {
        PhotoIndexer.createIndex(files, 
          THUMBNAIL_WIDTH, 
          THUMBNAIL_HEIGHT)
      }
    
    
    if (indexLoc < 0) {
      PhotoIndexer.saveIndex(index, new File("index.txt"), new File("Thumbnails"))
    }
    
    
    val target = files(0)
    
    
    
    val mosaic:BufferedImage = photoMosaicize(target, index, THUMBNAIL_WIDTH.min(sampleSize), THUMBNAIL_HEIGHT.min(sampleSize))
    ImageIO.write(mosaic,"png",new File("testmosaic.png"))
  }
  
  // TODO: use a class to represent the index
  def photoMosaicize(targetFile:File, 
    index:PhotoIndexer.PhotoIndex, 
    thumbnailWidth:Int, 
    thumbnailHeight:Int): BufferedImage = {
    
    val buffImage = ImageIO.read(targetFile)
    

    val patchSampleSize = sampleSize
    val patchWidth = patchSampleSize
    val patchHeight = patchSampleSize
        
    val numHorizontalPatches = buffImage.getWidth() / patchWidth
    val numVerticalPatches = buffImage.getHeight() / patchHeight
    
    val mosaic = new BufferedImage(thumbnailWidth * numHorizontalPatches, thumbnailHeight * numVerticalPatches, BufferedImage.TYPE_INT_RGB)
    val graphics2D = mosaic.createGraphics()

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
        

        var x2 = i * thumbnailWidth
        var y2 = j * thumbnailHeight
        val nearest = getNearestColorImage(avgImageColor, index)
        
        graphics2D.drawImage(nearest, x2, y2, thumbnailWidth, thumbnailHeight, null)

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
  
  

  // Calculates the BufferedImage with the single best 
  def getNearestColorImage(color: Color, photoIndex:PhotoIndexer.PhotoIndex): BufferedImage = {
    // Find image with closest average color
    val elements:Iterable[(BufferedImage,Color)] = photoIndex.values.map(x=>(x.thumbnail, x.avgColor))
    val closestElem = elements.reduceLeft(
          (e1,e2) => (if (color.distance(e1._2) < color.distance(e2._2)) e1 else e2)
    )
    closestElem._1
  }
  
  // /**
  //   * Calculates the 
  //   */
  //   def getNearestColorImages(targetColor: Color, colorMap:Map[BufferedImage, Color]): Seq[BufferedImage] = {
  //     val keys:List[BufferedImage] = colorMap.keys.toList
  //     keys.sort((c1,c2) =>
  //       colorMap(c1).distance(targetColor) < colorMap(c2).distance(targetColor)
  //     )
  //     keys
  //   }
 
  
  // Given a sequence of colors, sort them in order of proximity to the target
  // color
  def sortColors(colors:List[Color], targetColor:Color): List[Color] = {
    colors.sortWith( (c1,c2) =>
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
    math.sqrt(squared)
  }
 
  /** Currently define the distance function as the squared euclidean distance, but this can be changed*/
  val distance : (Color) => Double = euclideanDistanceSquared
 
  /** @return a Tuple3 where order is red, green, blue and numbers are in range 0..255 */
  def unpack(): Tuple3[Int,Int,Int] = {
    (original.getRed(), original.getGreen(), original.getBlue())
  }
}


}