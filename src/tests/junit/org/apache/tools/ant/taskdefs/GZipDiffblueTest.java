package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class GZipDiffblueTest {
  /**
   * Test {@link GZip#supportsNonFileResources()}.
   * <p>
   * Method under test: {@link GZip#supportsNonFileResources()}
   */
  @Test
  public void testSupportsNonFileResources() {
    // Arrange, Act and Assert
    assertTrue((new GZip()).supportsNonFileResources());
  }

  /**
   * Test new {@link GZip} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link GZip}
   */
  @Test
  public void testNewGZip() {
    // Arrange and Act
    GZip actualGZip = new GZip();

    // Assert
    assertNull(actualGZip.source);
    assertNull(actualGZip.zipFile);
    assertNull(actualGZip.getDescription());
    assertNull(actualGZip.getTaskName());
    assertNull(actualGZip.getTaskType());
    assertNull(actualGZip.getProject());
    assertNull(actualGZip.getOwningTarget());
    assertNull(actualGZip.getSrcResource());
  }
}
