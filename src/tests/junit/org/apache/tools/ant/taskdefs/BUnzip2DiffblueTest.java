package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class BUnzip2DiffblueTest {
  /**
   * Test {@link BUnzip2#getDefaultExtension()}.
   * <p>
   * Method under test: {@link BUnzip2#getDefaultExtension()}
   */
  @Test
  public void testGetDefaultExtension() {
    // Arrange, Act and Assert
    assertEquals(".bz2", (new BUnzip2()).getDefaultExtension());
  }

  /**
   * Test {@link BUnzip2#supportsNonFileResources()}.
   * <p>
   * Method under test: {@link BUnzip2#supportsNonFileResources()}
   */
  @Test
  public void testSupportsNonFileResources() {
    // Arrange, Act and Assert
    assertTrue((new BUnzip2()).supportsNonFileResources());
  }

  /**
   * Test new {@link BUnzip2} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link BUnzip2}
   */
  @Test
  public void testNewBUnzip2() {
    // Arrange and Act
    BUnzip2 actualBUnzip2 = new BUnzip2();

    // Assert
    assertEquals(".bz2", actualBUnzip2.getDefaultExtension());
    assertNull(actualBUnzip2.dest);
    assertNull(actualBUnzip2.source);
    assertNull(actualBUnzip2.getDescription());
    assertNull(actualBUnzip2.getTaskName());
    assertNull(actualBUnzip2.getTaskType());
    assertNull(actualBUnzip2.getProject());
    assertNull(actualBUnzip2.getOwningTarget());
    assertNull(actualBUnzip2.srcResource);
  }
}
