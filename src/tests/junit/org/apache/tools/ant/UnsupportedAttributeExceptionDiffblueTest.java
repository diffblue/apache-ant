package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class UnsupportedAttributeExceptionDiffblueTest {
  /**
   * Test {@link UnsupportedAttributeException#UnsupportedAttributeException(String, String)}.
   * <p>
   * Method under test: {@link UnsupportedAttributeException#UnsupportedAttributeException(String, String)}
   */
  @Test
  public void testNewUnsupportedAttributeException() {
    // Arrange and Act
    UnsupportedAttributeException actualUnsupportedAttributeException = new UnsupportedAttributeException("Msg",
        "Attribute");

    // Assert
    assertEquals("Attribute", actualUnsupportedAttributeException.getAttribute());
    assertEquals("Msg", actualUnsupportedAttributeException.getLocalizedMessage());
    assertEquals("Msg", actualUnsupportedAttributeException.getMessage());
    Location location = actualUnsupportedAttributeException.getLocation();
    assertNull(location.getFileName());
    assertNull(actualUnsupportedAttributeException.getCause());
    assertNull(actualUnsupportedAttributeException.getException());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualUnsupportedAttributeException.getSuppressed().length);
  }

  /**
   * Test {@link UnsupportedAttributeException#getAttribute()}.
   * <p>
   * Method under test: {@link UnsupportedAttributeException#getAttribute()}
   */
  @Test
  public void testGetAttribute() {
    // Arrange, Act and Assert
    assertEquals("Attribute", (new UnsupportedAttributeException("Msg", "Attribute")).getAttribute());
  }
}
