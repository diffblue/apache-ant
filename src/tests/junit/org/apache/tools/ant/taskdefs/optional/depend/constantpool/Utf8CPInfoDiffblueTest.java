package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class Utf8CPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Utf8CPInfo}
   *   <li>{@link Utf8CPInfo#toString()}
   *   <li>{@link Utf8CPInfo#getValue()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Utf8CPInfo actualUtf8CPInfo = new Utf8CPInfo();
    String actualToStringResult = actualUtf8CPInfo.toString();

    // Assert
    assertEquals("UTF8 Value = null", actualToStringResult);
    assertNull(actualUtf8CPInfo.getValue());
    assertEquals(1, actualUtf8CPInfo.getNumEntries());
    assertEquals(1, actualUtf8CPInfo.getTag());
    assertFalse(actualUtf8CPInfo.isResolved());
  }
}
