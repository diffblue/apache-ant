package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class UnsupportedElementExceptionDiffblueTest {
  /**
   * Test {@link UnsupportedElementException#UnsupportedElementException(String, String)}.
   * <p>
   * Method under test: {@link UnsupportedElementException#UnsupportedElementException(String, String)}
   */
  @Test
  public void testNewUnsupportedElementException() {
    // Arrange and Act
    UnsupportedElementException actualUnsupportedElementException = new UnsupportedElementException("Msg", "Element");

    // Assert
    assertEquals("Element", actualUnsupportedElementException.getElement());
    assertEquals("Msg", actualUnsupportedElementException.getLocalizedMessage());
    assertEquals("Msg", actualUnsupportedElementException.getMessage());
    Location location = actualUnsupportedElementException.getLocation();
    assertNull(location.getFileName());
    assertNull(actualUnsupportedElementException.getCause());
    assertNull(actualUnsupportedElementException.getException());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualUnsupportedElementException.getSuppressed().length);
  }

  /**
   * Test {@link UnsupportedElementException#getElement()}.
   * <p>
   * Method under test: {@link UnsupportedElementException#getElement()}
   */
  @Test
  public void testGetElement() {
    // Arrange, Act and Assert
    assertEquals("Element", (new UnsupportedElementException("Msg", "Element")).getElement());
  }
}
