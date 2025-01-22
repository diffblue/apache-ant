package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class SocketDiffblueTest {
  /**
   * Test {@link Socket#eval()}.
   * <ul>
   *   <li>Given {@link Socket} (default constructor) Port is zero.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Socket#eval()}
   */
  @Test
  public void testEval_givenSocketPortIsZero_thenThrowBuildException() throws BuildException {
    // Arrange
    Socket socket = new Socket();
    socket.setServer("foo");
    socket.setPort(0);

    // Act and Assert
    assertThrows(BuildException.class, () -> socket.eval());
  }

  /**
   * Test {@link Socket#eval()}.
   * <ul>
   *   <li>Given {@link Socket} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Socket#eval()}
   */
  @Test
  public void testEval_givenSocket_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Socket()).eval());
  }

  /**
   * Test new {@link Socket} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Socket}
   */
  @Test
  public void testNewSocket() {
    // Arrange and Act
    Socket actualSocket = new Socket();

    // Assert
    Location location = actualSocket.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSocket.getDescription());
    assertNull(actualSocket.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
