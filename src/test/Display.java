package org.nulleins.modart.modart;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Random;

public class Display extends JPanel implements ActionListener, Runnable {

  public Insets getInsets() {
    return new Insets(2, 2, 2, 2);
  }

  Image OSI;    // An off-screen images that holds the picture of the Mandelbrot set.
  // This is copied onto the drawing surface, if it exists.
  // It is created by the computational thread.

  Graphics OSG; // A graphics context for drawing on OSI.

  Thread runner;    // A thread to do the computation.
  boolean running;  // This is set to true when the thread is running.

  double xMin = -2.5;   // The ranges of x and y coordinates that
  double xMax = 1;      // are represented by this drawing surface
  double yMin = -1.25;
  double yMax = 1.25;

  /** Called by the system to paint the drawing surface; This copies the off-screen image
    * onto the screen, if the off-screen image exists.  If not, it just fills the drawing
    * surface with black. */

  @Override
  public void paintComponent(final Graphics g) {
    if (OSI == null) {
      g.setColor(Color.black);
      g.fillRect(0, 0, getWidth(), getHeight());
    } else {
      g.drawImage(OSI, 0, 0, null);
    }
  }

  /** This will be called when the user clicks on the "Start" or "Stop" button.
    * It responds by starting or stopping the animation. */
  @Override
  public void actionPerformed(final ActionEvent evt) {
    switch(evt.getActionCommand()) {
      case "Start":
        startRunning();
        break;
      case "Stop":
        stopRunning();
        break;
    }
  }

  /** Starts the computational thread, unless it is already running.
    * This should be impossible since this method is only called when the user clicks the
    * "Start" button, and that button is disabled when the thread is running.*/

   void startRunning() {
    if (!running) {
      runner = new Thread(this);
      // Creates a thread that will execute the run() method in this Display class.
      running = true;
      runner.start();
    }
  }

  /** Stops the computational thread. This is done by setting the value of the variable, running.
    * The thread checks this value regularly and will terminate when running becomes false.*/
  void stopRunning() {
    running = false;
  }

  /** The Mandelbrot set is represented by coloring each point (x,y) according to the number
    * of iterations it takes before the while loop in this method ends.  For points that are
    * within or very close to the Mandelbrot set, the count will reach the maximum value, 80.
    * These points will be colored purple. All other colors represent points that are definitely
    * NOT in the set.*/
  int countIterations(final double x, final double y) {
    int count = 0;
    double zx = x;
    double zy = y;
    while (count < 80 && Math.abs(x) < 100 && Math.abs(zy) < 100) {
      final double new_zx = zx * zx - zy * zy + x;
      zy = 2 * zx * zy + y;
      zx = new_zx;
      count++;
    }
    return count;
  }

  private static Random rand = new Random();
  private static int randInt(int min, int max) {
    return rand.nextInt((max - min) + 1) + min;
  }
  private static float randFloat(final float min, final float max) {
    return rand.nextFloat() * (max - min) + min;
  }

  /** The center pixel of a square that needs to be drawing. These variables are set in the run()
      method of the Display class and are used in the run() method of the artist object. The same
      is true for 'size' and 'colorIndex' */
  int xPix, yPix;
  int size;  // The size of the square that needs to be drawn.
  int colorIndex;  // A number between 1 and 80 that is used to decide on the color of the square.

  /** A Runnable object whose job is to paint a square onto the off-screen canvas, and then copy
    * that square onto the screen.  It will do this when its run method is called. The data for
    * the square are given by the preceding four variables. */
  final Runnable artist = new Runnable() {
    @Override
    public void run() {
      final int left = xPix - size / 2;
      final int top = yPix - size / 2;
      final float hue = colorIndex / 100.0f; //randFloat(10.0f, 100.0f);
      final float saturation = 1.0f; //randFloat(0.4f, 1.0f);
      final float brightness = 1.0f; //randFloat(0.4f, 1.0f);
      OSG.setColor(Color.getHSBColor(hue, saturation, brightness));
      OSG.fillRect(left, top, size, size);
      paintImmediately(left, top, size, size);
    }
  };

  /** This is the run method that is executed by the computational thread. It draws the Mandelbrot
    * set in a series of passes of increasing resolution. In each pass, it fills the applet with
    * squares that are colored to represent the Mandelbrot set. The size of the squares is cut in
    * half on each pass */
  @Override
  public void run() {
    int width = getWidth();   // Current size of this canvas.
    int height = getHeight();

    OSI = createImage(getWidth(), getHeight());
    // Create the off-screen image where the picture will
    // be stored, and fill it with black to start
    OSG = OSI.getGraphics();
    //OSG.setColor(Color.black);
    //OSG.fillRect(0, 0, width, height);

    drawImage(width, height);
    // The thread is about to end, either because the computation is finished or because running
    // has been set to false elsewhere.  In the former case, we have to set running = false here
    // to indicate that the thread is no longer running:
    running = false;
  }

  private void drawImage(final int width, final int height) {
    for (size = 64; size >= 1 && running; size = size / 2) {
      loopBody(width, height);
    }
  }

  /** @return pixel size as real coordinate */
  private double pixel2coordinate(final int p, final double bound) {
    return bound / p * size;
  }

  /** Outer loop performs one pass, filling the image with squares of the given size
    * (in pixels); Note that all loops end immediately if running becomes false */
  private void loopBody(final int width, final int height) {
    final double dx = pixel2coordinate(width, xMax - xMin);
    final double dy = pixel2coordinate(height, yMax - yMin);
    double x = xMin + dx / 2;  // x-coordinate of center of square
    for (xPix = size / 2; xPix < width + size / 2 && running; xPix += size) {
      x = drawSquares(height, dx, dy, x);
    }
  }

  /** Draw one column of squares */
  private double drawSquares(final int height, final double dx, final double dy, final double x) {
    double centerY = yMax - dy / 2;
    for (yPix = size / 2; yPix < height + size / 2 && running; yPix += size) {
      centerY = paintSquare(dy, x, centerY);
    }
    Thread.yield();  // Give other threads a chance to run
    return x + dx;
  }

  /** Paint one square, use iteration count to determine color, then invoke
    * the "artist" object to actually paint the square */
  private double paintSquare(final double dy, final double x, final double y) {
    colorIndex = countIterations(x, y);
    try {
      SwingUtilities.invokeAndWait(artist);
    } catch (Exception e) {
      // don't care!
    }
    return y - dy;
  }
}
