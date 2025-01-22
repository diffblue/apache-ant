package org.apache.tools.mail;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import java.io.IOException;
import org.junit.Test;

public class ErrorInQuitExceptionDiffblueTest {
  /**
   * Test {@link ErrorInQuitException#ErrorInQuitException(IOException)}.
   * <p>
   * Method under test: {@link ErrorInQuitException#ErrorInQuitException(IOException)}
   */
  @Test
  public void testNewErrorInQuitException() {
    // Arrange and Act
    ErrorInQuitException actualErrorInQuitException = new ErrorInQuitException(new IOException("foo"));

    // Assert
    assertEquals("foo", actualErrorInQuitException.getLocalizedMessage());
    assertEquals("foo", actualErrorInQuitException.getMessage());
    assertNull(actualErrorInQuitException.getCause());
    assertEquals(0, actualErrorInQuitException.getSuppressed().length);
  }
}
