package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertSame;
import org.junit.Test;

public class CustomJUnit4TestAdapterCacheDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Method under test: {@link CustomJUnit4TestAdapterCache#getInstance()}
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    CustomJUnit4TestAdapterCache actualInstance = CustomJUnit4TestAdapterCache.getInstance();

    // Assert
    assertSame(actualInstance, actualInstance.getInstance());
  }
}
