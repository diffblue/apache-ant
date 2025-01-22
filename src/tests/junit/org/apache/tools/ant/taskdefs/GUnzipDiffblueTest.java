package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class GUnzipDiffblueTest {
  /**
   * Test {@link GUnzip#getDefaultExtension()}.
   * <p>
   * Method under test: {@link GUnzip#getDefaultExtension()}
   */
  @Test
  public void testGetDefaultExtension() {
    // Arrange, Act and Assert
    assertEquals(".gz", (new GUnzip()).getDefaultExtension());
  }

  /**
   * Test {@link GUnzip#supportsNonFileResources()}.
   * <p>
   * Method under test: {@link GUnzip#supportsNonFileResources()}
   */
  @Test
  public void testSupportsNonFileResources() {
    // Arrange, Act and Assert
    assertTrue((new GUnzip()).supportsNonFileResources());
  }

  /**
   * Test new {@link GUnzip} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link GUnzip}
   */
  @Test
  public void testNewGUnzip() {
    // Arrange and Act
    GUnzip actualGUnzip = new GUnzip();

    // Assert
    assertEquals(".gz", actualGUnzip.getDefaultExtension());
    assertNull(actualGUnzip.dest);
    assertNull(actualGUnzip.source);
    assertNull(actualGUnzip.getDescription());
    assertNull(actualGUnzip.getTaskName());
    assertNull(actualGUnzip.getTaskType());
    assertNull(actualGUnzip.getProject());
    assertNull(actualGUnzip.getOwningTarget());
    assertNull(actualGUnzip.srcResource);
  }
}
