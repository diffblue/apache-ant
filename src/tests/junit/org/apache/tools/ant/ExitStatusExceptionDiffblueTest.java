package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import org.junit.Test;

public class ExitStatusExceptionDiffblueTest {
  /**
   * Test {@link ExitStatusException#ExitStatusException(int)}.
   * <p>
   * Method under test: {@link ExitStatusException#ExitStatusException(int)}
   */
  @Test
  public void testNewExitStatusException() {
    // Arrange and Act
    ExitStatusException actualExitStatusException = new ExitStatusException(1);

    // Assert
    assertNull(actualExitStatusException.getLocalizedMessage());
    assertNull(actualExitStatusException.getMessage());
    Location location = actualExitStatusException.getLocation();
    assertNull(location.getFileName());
    assertNull(actualExitStatusException.getCause());
    assertNull(actualExitStatusException.getException());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualExitStatusException.getSuppressed().length);
    assertEquals(1, actualExitStatusException.getStatus());
  }

  /**
   * Test {@link ExitStatusException#ExitStatusException(String, int)}.
   * <p>
   * Method under test: {@link ExitStatusException#ExitStatusException(String, int)}
   */
  @Test
  public void testNewExitStatusException2() {
    // Arrange and Act
    ExitStatusException actualExitStatusException = new ExitStatusException("Msg", 1);

    // Assert
    assertEquals("Msg", actualExitStatusException.getLocalizedMessage());
    assertEquals("Msg", actualExitStatusException.getMessage());
    Location location = actualExitStatusException.getLocation();
    assertNull(location.getFileName());
    assertNull(actualExitStatusException.getCause());
    assertNull(actualExitStatusException.getException());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualExitStatusException.getSuppressed().length);
    assertEquals(1, actualExitStatusException.getStatus());
  }

  /**
   * Test {@link ExitStatusException#ExitStatusException(String, int, Location)}.
   * <p>
   * Method under test: {@link ExitStatusException#ExitStatusException(String, int, Location)}
   */
  @Test
  public void testNewExitStatusException3() {
    // Arrange
    Location location = Location.UNKNOWN_LOCATION;

    // Act
    ExitStatusException actualExitStatusException = new ExitStatusException("An error occurred", 1, location);

    // Assert
    assertEquals("An error occurred", actualExitStatusException.getLocalizedMessage());
    assertEquals("An error occurred", actualExitStatusException.getMessage());
    assertNull(actualExitStatusException.getCause());
    assertNull(actualExitStatusException.getException());
    assertEquals(0, actualExitStatusException.getSuppressed().length);
    assertEquals(1, actualExitStatusException.getStatus());
    Location expectedLocation = location.UNKNOWN_LOCATION;
    assertSame(expectedLocation, actualExitStatusException.getLocation());
  }

  /**
   * Test {@link ExitStatusException#getStatus()}.
   * <p>
   * Method under test: {@link ExitStatusException#getStatus()}
   */
  @Test
  public void testGetStatus() {
    // Arrange, Act and Assert
    assertEquals(1, (new ExitStatusException(1)).getStatus());
  }
}
