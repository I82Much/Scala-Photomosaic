import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.RenderingHints
import java.awt.image.Raster
import java.awt.Rectangle
import java.util.HashMap
import java.awt.AlphaComposite

// Based on the article (but not source) from 
// http://www.drdobbs.com/184404848

// Logically a few steps:
// Create an index out of all the photos you want to consider; this index
// consists of a mapping from image file to average color in the image, as well as
// to the path of a saved thumbnail of the image.  Since the images will be
// rendered small in the final image, there's no need to deal with the full
// sized images.  This allows us to avoid a ton of I/O (we read all the thumbnails once,
// and then can store them all in memory while we create the photomosaic)
//
// For the source image, examine rectangular regions of the image.  Replace
// these regions with the picture calculated in the index with the most similar
// average color.
//
// Output the photomosaic to a file
//
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
  val FIELD_DELIMITER = ",";
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
          
          Console.println("File: " + file)
          
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


/**
* This trait allows client applications to be notified when the nearest images for a given chunk of
* our target image have been calculated.  This allows client applications to show a progress meter
* so that the user knows how long 
*/
trait PhotoMosaicCallback {
  def photosCalculated(row:Int, column:Int, photos:Seq[BufferedImage]): Unit
}

/**
* This trait logically encapsulates the act of finding the best match for a given segment of our 
* image
*/
trait DistanceProvider {
  def distance(targetImageSegment:Raster, image:BufferedImage):Double
  
  def getNearestImage(targetImageSegment:Raster, potentialMatches:Seq[BufferedImage]):BufferedImage = {
    // Basically, map each image to its distance, and then take the smallest distance.
    potentialMatches.min( 
      Ordering[Double].on[BufferedImage] ( distance(targetImageSegment, _) )
    )
  }
  
  // Sort the images based on their distance
  def getNearestImages(targetImageSegment:Raster, potentialMatches:Seq[BufferedImage]):Seq[BufferedImage] = {
    val keys = potentialMatches.toList
    val sortedKeys = keys.sortWith( (image1,image2) =>
      distance(targetImageSegment, image1) < distance(targetImageSegment, image2)
    )
    sortedKeys
  }
}


/**
 * This class uses the average color of the target raster to compare with the average color
 * of the potential images.  The image with the closest average color, in terms of euclidean distance
 * in RGB space is chosen.
 * 
 * Because it is very expensive to calculate the average color of each image, this is computed
 * once and used as a lookup table for efficiency reasons.
 *
 * This is the deafult distance provider and can create good photomosaics.  Unfortunately it's not perfect,
 * as the 'average' color of an image can be very different from some parts of that image.  For instance,
 * if one of the target images is mostly sky and some mountain on the bottom, the average color of the image
 * will be a blue.  If this is used repeatedly in a blue section in the target image, then we will have
 * strange stripes, and distracting artifacts.
 */
class SimpleColorDistanceProvider(colorMap:Map[BufferedImage, Color]) extends DistanceProvider {
  
  implicit def color2RichColor(x: Color) = new RichColor(x)
  
  
  /**
   * returns a sorted list of buffered images, where the first element is the
   * closest in average color to the target color, and the last image is
   * the furthest away
   */
   override def distance(targetImageSegment:Raster, image:BufferedImage):Double = {
     val targetColor:Color = PhotoMosaic.calculateColorFromRaster(targetImageSegment)
     // TODO: handle when the map doesn't have htis color
     val imageColor:Color = colorMap.get(image).get
     targetColor.distance(imageColor)
   }

   // override def getNearestImages(targetImageSegment:Raster, potentialMatches:Seq[BufferedImage]):Seq[BufferedImage] = {
   //      val targetColor:Color = PhotoMosaic.calculateColorFromRaster(targetImageSegment)
   //      val keys = potentialMatches.toList
   //      val sortedKeys = keys.sortWith( (image1,image2) =>
   //        distance(targetImageSegment, image1) < distance(targetImageSegment, image2)
   //      )
   //      sortedKeys
   //    }
  
}





object PhotoMosaic {
  
  implicit def color2RichColor(x: Color) = new RichColor(x)
  
  type ImageGrid = Array[Array[Seq[BufferedImage]]]
  
  val sampleSize = 20
  
  val THUMBNAIL_WIDTH = 640//80//160
  val THUMBNAIL_HEIGHT = 480//60//120
  
  // Allow a single image to repeat at most 5 times; if negative allow
  // infinite number of repetitions
  val MAX_NUM_REPETITIONS = -1//5

  
  // TODO: Add command line arguments
  // -i, in, index = index file
  // -t, target = target image file (must be a format that ImageIO can read)
  // -pix [picture1 .. pictureN]
  def main(args: Array[String]):Unit = {
    if (args.length == 0) {
      Console.println("Usage: ")
      Console.println("PhotoMosaic indexing file1 ... fileN")
      System.exit(1)
    }
    
    val INDEXING = true // args(0).toBoolean
    
    
    if (INDEXING) {
      val thumbnailWidth = THUMBNAIL_WIDTH
      val thumbnailHeight = THUMBNAIL_HEIGHT
    
      val files = args.map(new File(_))
      val indexLoc = files.findIndexOf(_.getName().contains(".txt"))
    
      val target = files.filter(_.getName().toLowerCase().endsWith(".jpg"))(0)
    
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
        System.exit(1)
      }
    }
    
    
    else {
      // val thumbnailWidth = Integer.parseInt(args(0))
      //       val thumbnailHeight = Integer.parseInt(args(1))
      //       val index:PhotoIndexer.PhotoIndex = PhotoIndexer.loadIndex(new File(args(2)))
      //       val target = new File(args(3))
      
      val index:PhotoIndexer.PhotoIndex = PhotoIndexer.loadIndex(new File(args(0)))
      val images:Array[String] = args.tail
      

      while (true) {
        val defaultNumColumns = 120//80//40  
        val defaultNumRows = 90//60//30
        val defaultImageWidth = 3648*2
        val defaultImageHeight = 2736*2
        
                  
        Console.println("Pick an image:")  
        Console.println(images.zipWithIndex.map(x=>x._2 + "\t" + x._1).mkString("\n"))
        val indexNumber = Console.readInt()
        val target:File = new File(images(indexNumber))
          
        Console.println("Using " + target)  
        Console.print("Number of columns: (" + defaultNumColumns + ")")  
        val numColumns:Int = defaultNumColumns//Console.readInt()  
        Console.print("Number of rows: ( " + defaultNumRows + ")")  
        val numRows = defaultNumRows//Console.readInt()  
                  
        Console.print("Image Width: ")
        // val width = Console.readInt()
        val width = defaultImageWidth 
        
        Console.print("Image Height: ")
        // val height = Console.readInt()
        val height = defaultImageHeight
        
        
        // Console.print("Num repetitions: ")
        
        Console.print("Opacity:")
        val opacity = Console.readFloat()
        
        val numRepetitions = -1 //Console.readInt()
        
        Console.print("Num closest images to consider:")
        val numClosest = Console.readInt()
        
        
        Console.print("Output name: ")
        val outputName = Console.readLine().trim()
      
      
        val targetImage:BufferedImage = ImageIO.read(target)
        
        
        val mosaic:BufferedImage = photoMosaicize(targetImage, index, opacity, width, height, numRows, numColumns, numClosest, numRepetitions)
        ImageIO.write(mosaic,"jpg",new File(outputName))
        
        Console.println("Finished.  Again?")
      }
    }
  }
  
  

  
  def createMosaic(targetImage:BufferedImage, 
      index:PhotoIndexer.PhotoIndex,
      opacity:Float, 
      targetWidth:Int,
      targetHeight:Int,
      numRows:Int,
      numColumns:Int, callback:PhotoMosaicCallback): ImageGrid = {
          
          var indexCopy = index

          // Map from the buffered image to that image's average color
          var colorMap:Map[BufferedImage,Color] = 
          index.values.map(data => (data.thumbnail, data.avgColor)).toMap

          // We look at rectangular regions of the target image, calculate their average
          // colors, and then pick images that match those colors.
          val sampleWidth = targetImage.getWidth / numColumns
          val sampleHeight = targetImage.getHeight / numRows

          // Used to report the progress of the process
          var counter = 1
          val numSubImages = numRows * numColumns
          
          val imageGrid:ImageGrid = Array.fill(numRows, numColumns)(Nil)
          
          // for each patch in the image
          for (row <- 0 until numRows) {
            for (column <- 0 until numColumns) {
              val x = column * sampleWidth
              val y = row * sampleHeight
              // This is the small rectangular region of the target image that we're 
              // currently considering
              val subImage = targetImage.getData(new Rectangle(x,y,sampleWidth,sampleHeight))
              val avgImageColor = calculateColorFromRaster(subImage)

              val nearest:Seq[BufferedImage] = getNearestColorImages(avgImageColor, colorMap)
              
              imageGrid(row)(column) = nearest
              
              callback.photosCalculated(row, column, nearest)
              
              val percent = 100.0 * counter / numSubImages
              // TODO: for GUI version, use a display bar
              if (counter % 100 == 0) {
                println(percent + " completed (" + counter + " of" + numSubImages + ")")
              }
              counter+=1
            }
          }
          imageGrid
  }
  
  
  /**
  * @param targetFile
  * @param index
  * @param targetWidth how wide should the final image be, in pixels
  * @param targetHeight 
  */
  def photoMosaicize(targetImage:BufferedImage, 
    index:PhotoIndexer.PhotoIndex,
    opacity:Float, 
    targetWidth:Int,
    targetHeight:Int,
    numRows:Int,
    numColumns:Int,
    imagesToChooseFrom:Int,
    maxNumRepetitions:Int): BufferedImage = {
    
    var indexCopy = index

    // Keep track of how many times each thumbnail has been used
    val repetitionMap = new HashMap[BufferedImage, java.lang.Integer]
    index.values.foreach { x => repetitionMap.put(x.thumbnail, 0) }
    
    val repetitionLimited = maxNumRepetitions > 0
    
    // Map from the buffered image to the file that contains the buffered image
    val buffImageToFile:Map[BufferedImage, File] = indexCopy.iterator.map(x => (x._2.thumbnail, x._1)).toMap
    
    // Map from the buffered image to that image's average color
    var colorMap:Map[BufferedImage,Color] = 
        index.values.map(data => (data.thumbnail, data.avgColor)).toMap
        
    
    val thumbnailWidth = targetWidth / numColumns
    val thumbnailHeight = targetHeight / numRows
    
    // Due to integer truncation, the desired width/height might not perfectly line
    // up with our grid.  Fudge the numbers slightly to allow the grid to be perfect
    val productWidth = thumbnailWidth * numColumns
    val productHeight = thumbnailHeight * numRows
    
    // We look at rectangular regions of the target image, calculate their average
    // colors, and then pick images that match those colors.
    val sampleWidth = targetImage.getWidth / numColumns
    val sampleHeight = targetImage.getHeight / numRows
    
    // This is what we're creating
    val mosaic:BufferedImage = new BufferedImage(productWidth, productHeight, BufferedImage.TYPE_INT_RGB)
    val graphics2D = mosaic.createGraphics()

    // Used to report the progress of the process
    var counter = 1
    val numSubImages = numRows * numColumns
    
    val random = new java.util.Random()
    
    if (opacity > 0) {
        // draw the original image in the background
        graphics2D.drawImage(targetImage, 0, 0, productWidth, productHeight, null)
        graphics2D.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity))
    }
    // for each patch in the image
    for (row <- 0 until numRows) {
      for (column <- 0 until numColumns) {
        val x = column * sampleWidth
        val y = row * sampleHeight
        // This is the small rectangular region of the target image that we're 
        // currently considering
        val subImage = targetImage.getData(new Rectangle(x,y,sampleWidth,sampleHeight))
        val avgImageColor = calculateColorFromRaster(subImage)
        
        // The x,y coordinate of upper left of subimage in our target mosaic
        var x2 = column * thumbnailWidth
        var y2 = row * thumbnailHeight
        
        
        val nearest:BufferedImage = 
            if (imagesToChooseFrom > 1) {
                val closestImages:Seq[BufferedImage] = getNearestColorImages(avgImageColor, colorMap)
                
                val closestFiles:Seq[File] = closestImages.map(buffImageToFile.get(_).get)
                
                // Console.println("Average image color for row " + row + " column " + column + " is " + avgImageColor)
                // Console.println("Closest images: " + closestFiles)
                
                val randomIndex = random.nextInt(imagesToChooseFrom)
                // Console.println("random index and image " + randomIndex + ", " + closestFiles(randomIndex))
                // Console.println()
                
                // Randomly pick from among the n closest images
                closestImages(randomIndex)
                
            }
            else {
                getNearestColorImage(avgImageColor, indexCopy)
            }
        
        if (repetitionLimited) {
          val numTimesRepeated:Int = repetitionMap.get(nearest).intValue + 1
          repetitionMap.put(nearest, numTimesRepeated)
          
          if (numTimesRepeated >= maxNumRepetitions) {
            val nearestFile = buffImageToFile.get(nearest)
            println("Removing " + nearestFile + " from consideration; it has been used " + numTimesRepeated + " times")
            indexCopy -= nearestFile.get
          }
        }        
        val DRAW_AVG_COLOR_BLOCKS = false

        
        if (DRAW_AVG_COLOR_BLOCKS) {
            // This way merely draws the block of average color
            graphics2D.setColor(avgImageColor)
            graphics2D.fillRect(x2,y2,thumbnailWidth,thumbnailHeight)
        }
        // else {
            // We have found the nearest color image, draw it into the mosaic
            graphics2D.drawImage(nearest, x2, y2, thumbnailWidth, thumbnailHeight, null)
        // }

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
  
  /**
   * returns a sorted list of buffered images, where the first element is the
   * closest in average color to the target color, and the last image is
   * the furthest away
   */
    def getNearestColorImages(targetColor:Color, colorMap:Map[BufferedImage,Color]): Seq[BufferedImage] = {
      val keys = colorMap.keys.toList
      val sortedKeys = keys.sortWith( (image1,image2) =>
        colorMap(image1).distance(targetColor) < colorMap(image2).distance(targetColor)
      )
      sortedKeys
    }
 
  
  // Calculates the average color of a Raster object
  // A raster is an abstraction of a piece of an image and the underlying
  // pixel data.
  // For instance, we can get a raster than is of the upper left twenty
  // pixel square of an image.
  // 
  def calculateColorFromRaster(raster:Raster): Color = {
    // Important: If we don't keep the sums as Longs, they can easily
    // overflow when processing a large, overexposed image
    var redSum:Long = 0
    var greenSum:Long = 0
    var blueSum:Long = 0
  
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
    
    val red:Int = (redSum / numPixels).asInstanceOf[Int]
    val green:Int = (greenSum / numPixels).asInstanceOf[Int]
    val blue:Int = (blueSum / numPixels).asInstanceOf[Int]

    new Color(red, green, blue)
  }
  

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