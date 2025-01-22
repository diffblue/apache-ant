package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class ExitExceptionDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ExitException#ExitException(String, int)}
   *   <li>{@link ExitException#getStatus()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ExitException actualExitException = new ExitException("Msg", 1);
    int actualStatus = actualExitException.getStatus();

    // Assert
    assertEquals("Msg", actualExitException.getMessage());
    assertNull(actualExitException.getCause());
    assertEquals(0, actualExitException.getSuppressed().length);
    assertEquals(1, actualStatus);
  }

  /**
   * Test {@link ExitException#ExitException(int)}.
   * <p>
   * Method under test: {@link ExitException#ExitException(int)}
   */
  @Test
  public void testNewExitException() {
    // Arrange and Act
    ExitException actualExitException = new ExitException(1);

    // Assert
    assertEquals("ExitException: status 1", actualExitException.getLocalizedMessage());
    assertEquals("ExitException: status 1", actualExitException.getMessage());
    assertNull(actualExitException.getCause());
    assertEquals(0, actualExitException.getSuppressed().length);
    assertEquals(1, actualExitException.getStatus());
  }
}
