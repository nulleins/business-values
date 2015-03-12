package org.nulleins.modart.modart;

import javax.swing.*;
import java.awt.*;

public class TestDataGenerator {

  private static void createAndShowGUI() {
    JFrame frame = new JFrame("Generate Test Payments");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setBackground(Color.gray);

    final Display content = new Display();
    frame.setBackground(Color.gray);

    frame.setPreferredSize(new Dimension(100, 100));
    frame.add(content, BorderLayout.CENTER);

    //Display the window.
    frame.pack();
    frame.setVisible(true);
    content.startRunning();
  }

  public static void main(String[] args) {
    javax.swing.SwingUtilities.invokeLater(TestDataGenerator::createAndShowGUI);
  }
}
