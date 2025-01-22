package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class BZip2DiffblueTest {
  /**
   * Test {@link BZip2#supportsNonFileResources()}.
   * <p>
   * Method under test: {@link BZip2#supportsNonFileResources()}
   */
  @Test
  public void testSupportsNonFileResources() {
    // Arrange, Act and Assert
    assertTrue((new BZip2()).supportsNonFileResources());
  }

  /**
   * Test new {@link BZip2} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link BZip2}
   */
  @Test
  public void testNewBZip2() {
    // Arrange and Act
    BZip2 actualBZip2 = new BZip2();

    // Assert
    assertNull(actualBZip2.source);
    assertNull(actualBZip2.zipFile);
    assertNull(actualBZip2.getDescription());
    assertNull(actualBZip2.getTaskName());
    assertNull(actualBZip2.getTaskType());
    assertNull(actualBZip2.getProject());
    assertNull(actualBZip2.getOwningTarget());
    assertNull(actualBZip2.getSrcResource());
  }
}
