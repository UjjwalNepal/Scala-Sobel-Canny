import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.Color
import scala.math.sqrt
import scala.math.pow
import scala.math.floor
object Main{

  var upperThreshold = 0.0
  var lowerThreshold = 0.0
  def main(args: Array[String]) : Unit ={
    test()
  }
  
  def pixelsToGrayScale(red:Double,green:Double,blue:Double): Double = (red * 0.33 + green * 0.33 + blue * 0.33 )
  
  def phototest(img: BufferedImage): BufferedImage = {
  // obtain width and height of image
  val w = img.getWidth
  val h = img.getHeight
  var sum = 0
  var count = 0
  for {
      w1 <- (0 until w).toVector
      h1 <- (0 until h).toVector
  } yield {
    count = count + 1
     val col = img.getRGB(w1, h1)
         val red = (col & 0xff0000)/65536
         val green = (col & 0xff00)/256
         val blue = (col & 0xff)
         val graycol = pixelsToGrayScale(red, green, blue).toInt
        sum = sum + graycol
      img.setRGB(w1, h1, new Color(graycol,graycol,graycol).getRGB)
     
  }
    val avg = sum / count
    //upperThreshold = avg + 0.25 * avg
    upperThreshold = 100
    lowerThreshold = 70
    println(upperThreshold)
    println(lowerThreshold)
    img
  }
  
  def gaussianBlur(img:BufferedImage):BufferedImage = {
    val w = img.getWidth
    val h = img.getHeight
    var newImage = img


//    var image = Array.ofDim[Int](w-2,h-2)
    
    var kernel = Array.ofDim[Int](3,3)
    kernel = Array(Array(1,2,1),Array(2,4,2),Array(1,2,1))

    for {
        i <- (1 until w-1)
        j <- (1 until h-1)
    }yield {
      var sum = 0

      for{
        k <- (-1 until 2)
        l <- (-1 until 2)
      }yield{
        val a = img.getRGB(i+k,j+l)
        val data = (a & 0xff0000)/65536
        val p = kernel(k+1)(l+1)
        sum = sum + (p * data)
        //println("i:"+i+",j:"+j+",k:"+(k+1)+",l:"+(l+1))

      }
      newImage.setRGB(i,j,new Color(sum/16,sum/16,sum/16).getRGB)
    }
    newImage


  }

  def sobelFilter(img: BufferedImage):BufferedImage = {
    val w = img.getWidth
    val h = img.getHeight
    var newAngle = 0
    var newImage:BufferedImage = new BufferedImage(w,h,BufferedImage.TYPE_INT_RGB)

    var directionArray = Array.ofDim[Double](w,h)
    var magnitudeArray = Array.ofDim[Double](w,h)
    //BufferedImage newImage  = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)

    val rxKernel = Array(Array(1,0,-1),Array(2,0,-2),Array(1,0,-1))
    val ryKernel = Array(Array(1,2,1),Array(0,0,0),Array(-1,-2,-1))


    for {
      i <- (1 until w-1)
      j <- (1 until h-1)
    }yield {
      var sumx = 0.0f
      var sumy = 0.0f
      var newSum = 0.0f
      for{
        k <- (-1 until 2)
        l <- (-1 until 2)
      }yield{
        val a = img.getRGB(i+k,j+l)
        val data = (a & 0xff)
        val px = rxKernel(k+1)(l+1)
        val py = ryKernel(k+1)(l+1)
        sumx = sumx + (px * data)
        sumy = sumy + (py * data)

        //println("i:"+i+",j:"+j+",k:"+(k+1)+",l:"+(l+1))

      }
      var direction = Math.atan(sumy/sumx) * (180/Math.PI)
      if(direction<0){
        direction = 180 + direction
      }else if(direction.isNaN){
        direction = 0
      }

      newSum = sqrt(pow(sumx,2) + pow(sumy,2)).toFloat


      if(newSum > 255){
        newSum = 255
      }

      if (((direction < 22.5) && (direction > -22.5)) || (direction > 157.5) || (direction < -157.5)) newAngle = 0
      if (((direction > 22.5) && (direction < 67.5)) || ((direction < -112.5) && (direction > -157.5))) newAngle = 45
      if (((direction > 67.5) && (direction < 112.5)) || ((direction < -67.5) && (direction > -112.5))) newAngle = 90
      if (((direction > 112.5) && (direction < 157.5)) || ((direction < -22.5) && (direction > -67.5))) newAngle = 135
      magnitudeArray(i)(j) = newSum
      directionArray(i)(j) = newAngle

        newImage.setRGB(i,j,new Color(newSum.toInt,newSum.toInt,newSum.toInt,255).getRGB)
    }
    ImageIO.write(newImage, "jpg", new File("sobel.jpg"))

    for {
      i <- (1 until w-1)
      j <- (1 until h-1)
    }yield{
      var cannySum = 0.0

        directionArray(i)(j) match {
          case 0 => {
            if(!columnEnd(1,j) || !rowEnd(0,i)){
              if(calculateMax(magnitudeArray(i)(j),magnitudeArray(i)(j-1),magnitudeArray(i)(j+1))){
                if(magnitudeArray(i)(j) > upperThreshold){
                  magnitudeArray(i)(j) = 255
                }else {
                  if(magnitudeArray(i)(j) > lowerThreshold){
                    if(directionArray(i)(j) == directionArray(i)(j-1) || directionArray(i)(j) == directionArray(i)(j+1)){
                      magnitudeArray(i)(j) = 255
                    }else{
                      magnitudeArray(i)(j)=0
                    }
                  }else{
                    magnitudeArray(i)(j)=0
                  }
                }
              }else{
                magnitudeArray(i)(j) = 0
              }
            }

          }
          case 45 => {
            if(!columnEnd(1,j) || !rowEnd(1,i)){
              if(calculateMax(magnitudeArray(i)(j),magnitudeArray(i-1)(j+1),magnitudeArray(i+1)(j-1))){
                if(magnitudeArray(i)(j) > upperThreshold){
                  magnitudeArray(i)(j) = 255
                }else {
                  if(magnitudeArray(i)(j) > lowerThreshold){
                    if(directionArray(i)(j) == directionArray(i-1)(j+1) || directionArray(i)(j) == directionArray(i+1)(j-1)){
                      magnitudeArray(i)(j) = 255
                    }else{
                      magnitudeArray(i)(j)=0
                    }
                  }else{
                    magnitudeArray(i)(j)=0
                  }
                }
              }else{
                magnitudeArray(i)(j) = 0
              }
            }

          }
          case 90 => {
            if(!columnEnd(0,j) || !rowEnd(1,i)){
              if(calculateMax(magnitudeArray(i)(j),magnitudeArray(i-1)(j),magnitudeArray(i+1)(j))){
                if(magnitudeArray(i)(j) > upperThreshold){
                  magnitudeArray(i)(j) = 255
                }else {
                  if(magnitudeArray(i)(j) > lowerThreshold){
                    if(directionArray(i)(j) == directionArray(i-1)(j) || directionArray(i)(j) == directionArray(i+1)(j)){
                      magnitudeArray(i)(j) = 255
                    }else{
                      magnitudeArray(i)(j)=0
                    }
                  }else{
                    magnitudeArray(i)(j)=0
                  }
                }
              }else{
                magnitudeArray(i)(j) = 0
              }
            }

          }
          case 135 => {
            if(!columnEnd(-1,j) || !rowEnd(1,i)){
              if(calculateMax(magnitudeArray(i)(j),magnitudeArray(i-1)(j-1),magnitudeArray(i+1)(j+1))){
                if(magnitudeArray(i)(j) > upperThreshold){
                  magnitudeArray(i)(j) = 255
                }else {
                  if(magnitudeArray(i)(j) > lowerThreshold){
                    if(directionArray(i)(j) == directionArray(i-1)(j-1) || directionArray(i)(j) == directionArray(i+1)(j+1)){
                      magnitudeArray(i)(j) = 255
                    }else{
                      magnitudeArray(i)(j)=0
                    }
                  }else{
                    magnitudeArray(i)(j)=0
                  }
                }
              }else{
                magnitudeArray(i)(j) = 0
              }
            }

          }
        }


      cannySum = magnitudeArray(i)(j)
      newImage.setRGB(i,j,new Color(cannySum.toInt,cannySum.toInt,cannySum.toInt,255).getRGB)
    }


    def columnEnd(colShift:Int,column:Int):Boolean = {

        if(colShift < 0){
          if(column > 0){
            return false
          }
        }else{
          if(column < w - 1){
            return false
          }
        }
      true
    }

    def rowEnd(rowShift:Int, row:Int):Boolean ={
      if(rowShift < 0){
        if(row > 0){
          return false
        }
      }else{
        if(row < h - 1){
          return false
        }
      }
      true
    }

    def calculateMax(current:Double,x:Double,y:Double): Boolean ={
      if(current > x){
        if(current > y){
          return true
        }
      }
      false
    }


    newImage
  }







  
def test() {
  
  val photo1 = ImageIO.read(new File("/Users/otonomis/Desktop/PUBG.jpg"))

  val photo2 = phototest(photo1)

  ImageIO.write(photo2, "jpg", new File("graysccale.jpg"))
  val gaussianBlurred = gaussianBlur(photo2)



  // save image to file "test.jpg"
  ImageIO.write(gaussianBlurred, "jpg", new File("test.jpg"))

  val sobelImage = sobelFilter(gaussianBlurred)
  ImageIO.write(sobelImage, "jpg", new File("canny.jpg"))

}


}

