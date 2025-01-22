package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class ManifestExceptionDiffblueTest {
  /**
   * Test {@link ManifestException#ManifestException(String)}.
   * <p>
   * Method under test: {@link ManifestException#ManifestException(String)}
   */
  @Test
  public void testNewManifestException() {
    // Arrange and Act
    ManifestException actualManifestException = new ManifestException("Msg");

    // Assert
    assertEquals("Msg", actualManifestException.getMessage());
    assertNull(actualManifestException.getCause());
    assertEquals(0, actualManifestException.getSuppressed().length);
  }
}
