package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import java.io.IOException;
import org.apache.ant.antunit.AntUnit;
import org.apache.tools.ant.taskdefs.LogOutputStream;
import org.junit.Test;

public class LineOrientedOutputStreamDiffblueTest {
  /**
  * Method under test: {@link LineOrientedOutputStream#close()}
  */
  @Test
  public void testClose() throws IOException {
    // Arrange
    LogOutputStream logOutputStream = new LogOutputStream(new AntUnit());

    // Act
    logOutputStream.close();

    // Assert that nothing has changed
    assertEquals(2, logOutputStream.getMessageLevel());
  }

  /**
   * Method under test: {@link LineOrientedOutputStream#close()}
   */
  @Test
  public void testClose2() throws IOException {
    // Arrange
    LogOutputStream logOutputStream = new LogOutputStream(new AntUnit());
    logOutputStream.write(19088743);

    // Act
    logOutputStream.close();

    // Assert that nothing has changed
    assertEquals(2, logOutputStream.getMessageLevel());
  }

  /**
   * Method under test: {@link LineOrientedOutputStream#flush()}
   */
  @Test
  public void testFlush() throws IOException {
    // Arrange
    LogOutputStream logOutputStream = new LogOutputStream(new AntUnit());

    // Act
    logOutputStream.flush();

    // Assert that nothing has changed
    assertEquals(2, logOutputStream.getMessageLevel());
  }

  /**
   * Method under test: {@link LineOrientedOutputStream#processLine(byte[])}
   */
  @Test
  public void testProcessLine() throws IOException {
    // Arrange
    LogOutputStream logOutputStream = new LogOutputStream(new AntUnit());

    // Act
    logOutputStream.processLine("AAAAAAAA".getBytes("UTF-8"));

    // Assert that nothing has changed
    assertEquals(2, logOutputStream.getMessageLevel());
  }

  /**
   * Method under test: {@link LineOrientedOutputStream#write(byte[], int, int)}
   */
  @Test
  public void testWrite() throws IOException {
    // Arrange
    LogOutputStream logOutputStream = new LogOutputStream(new AntUnit());

    // Act
    logOutputStream.write(new byte[]{'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'}, 1, 0);

    // Assert that nothing has changed
    assertEquals(2, logOutputStream.getMessageLevel());
  }

  /**
   * Method under test: {@link LineOrientedOutputStream#write(byte[], int, int)}
   */
  @Test
  public void testWrite2() throws IOException {
    // Arrange
    LogOutputStream logOutputStream = new LogOutputStream(new AntUnit());

    // Act
    logOutputStream.write("AAAAAAAA".getBytes("UTF-8"), 3, 3);

    // Assert that nothing has changed
    assertEquals(2, logOutputStream.getMessageLevel());
  }
}

